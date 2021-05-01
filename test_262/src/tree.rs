use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::hash::Hash;
use std::iter::Peekable;

#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Tree<Id: Hash + Eq + Ord + Default, T> {
    Leaf(T),
    Branch(BTreeMap<Id, Tree<Id, T>>),
}

impl<Id: Hash + Eq + Ord + ToOwned<Owned = Id> + Default, T> Default for Tree<Id, T> {
    fn default() -> Self {
        Tree::new()
    }
}

const IS_LEAF: bool = false;
const IS_BRANCH: bool = true;

impl<T, Id: Hash + Eq + Ord + ToOwned<Owned = Id> + Default> Tree<Id, T> {
    pub fn new() -> Self {
        Tree::Branch(BTreeMap::new())
    }

    pub(crate) fn insert<TId: Into<Id>, TIterator: Iterator<Item = TId>>(
        &mut self,
        path: TIterator,
        item: T,
    ) -> Option<T> {
        self.insert_internal(path.peekable(), item)
    }

    pub(crate) fn get<TId: Into<Id>, TIterator: Iterator<Item = TId>>(
        &mut self,
        path: TIterator,
    ) -> Option<&mut T> {
        self.get_internal(path.peekable())
    }

    fn insert_internal<TId: Into<Id>, TIterator: Iterator<Item = TId>>(
        &mut self,
        mut segments: Peekable<TIterator>,
        item: T,
    ) -> Option<T> {
        if let Some(segment) = segments.next() {
            match (self, segments.peek().is_some()) {
                (Tree::Branch(mappings), IS_LEAF) => {
                    let previous = mappings.insert(segment.into(), Tree::Leaf(item));

                    if let Some(Tree::Leaf(value)) = previous {
                        Some(value)
                    } else {
                        None
                    }
                }
                (Tree::Branch(mappings), IS_BRANCH) => mappings
                    .entry(segment.into())
                    .or_insert_with(|| Tree::Branch(BTreeMap::new()))
                    .insert_internal(segments, item),
                (Tree::Leaf(existing), IS_LEAF) => Some(std::mem::replace(existing, item)),
                (node, IS_BRANCH) => {
                    let mut new_child = Tree::Branch(BTreeMap::new());
                    let result = new_child.insert_internal(segments, item);

                    let mut map = BTreeMap::new();
                    map.insert(segment.into(), new_child);

                    *node = Tree::Branch(map);

                    result
                }
            }
        } else {
            unreachable!("Ohno")
        }
    }

    fn get_internal<TId: Into<Id>, TIterator: Iterator<Item = TId>>(
        &mut self,
        mut segments: TIterator,
    ) -> Option<&mut T> {
        match (self, segments.next()) {
            (Tree::Branch(nodes), Some(segment)) => nodes
                .get_mut(&segment.into())
                .and_then(|node| node.get_internal(segments)),
            (Tree::Branch(_), None) => None,
            (Tree::Leaf(value), Some(segment)) => None,
            (Tree::Leaf(value), None) => Some(value),
        }
    }

    pub(crate) fn reduce<TR>(
        &self,
        map_leaf: fn(usize, &Id, &T) -> TR,
        map_branch: fn(usize, &Id, &[TR]) -> TR,
    ) -> TR {
        self.reduce_internal(0, &Default::default(), map_leaf, map_branch)
    }

    pub(crate) fn reduce_internal<TR>(
        &self,
        depth: usize,
        key: &Id,
        map_leaf: fn(usize, &Id, &T) -> TR,
        map_branch: fn(usize, &Id, &[TR]) -> TR,
    ) -> TR {
        match self {
            Tree::Leaf(leaf) => map_leaf(depth, key, leaf),
            Tree::Branch(branches) => {
                let results: Vec<TR> = branches
                    .iter()
                    .map(|(key, branch)| {
                        branch.reduce_internal(depth + 1, key, map_leaf, map_branch)
                    })
                    .collect();

                map_branch(depth, key, &results)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::tree::Tree;
    use crate::tree::Tree::Leaf;

    macro_rules! branch {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::BTreeMap::new();
         $( map.insert($key.into(), $val); )*
         Tree::Branch(map)
    }}
}

    #[test]
    fn test_insert_nested() {
        let mut tree: Tree<String, bool> = Tree::new();

        tree.insert("test1/test2/leaf".split('/'), true);
        tree.insert("test1/test2/leaf2".split('/'), false);

        assert_eq!(
            branch! {
                "test1" => branch! {
                    "test2" => branch! {
                        "leaf" => Leaf(true),
                        "leaf2" => Leaf(false)
                    }
                }
            },
            tree
        )
    }

    #[test]
    fn test_node_conversion_to_leaf() {
        let mut tree: Tree<String, bool> = Tree::new();

        tree.insert(&mut "test1/test2/leaf1".split('/'), true);
        tree.insert("test1".split('/'), false);

        assert_eq!(branch! { "test1" => Leaf(false) }, tree)
    }

    #[test]
    fn test_node_conversion_to_branch() {
        let mut tree: Tree<String, bool> = Tree::new();

        tree.insert("test1".split('/'), false);
        tree.insert("test1/test2/leaf2".split('/'), true);

        assert_eq!(
            branch! {
                "test1" => branch! {
                    "test2" => branch! {
                        "leaf2" => Tree::Leaf(true)
                    }
                }
            },
            tree
        )
    }

    #[test]
    fn test_replace_value() {
        let mut tree: Tree<String, bool> = Tree::new();

        tree.insert("test1/test2/leaf2".split('/'), false);

        assert_eq!(
            tree.insert("test1/test2/leaf2".split('/'), true),
            Some(false)
        );
    }

    #[test]
    fn test_get_value() {
        let mut tree: Tree<String, bool> = Tree::new();

        tree.insert("test1".split('/'), false);
        tree.insert("test1/test2/leaf2".split('/'), true);

        assert_eq!(
            tree.get("test1/test2/leaf2".split('/')).cloned(),
            Some(true)
        );
    }
}
