use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{BuildHasherDefault, Hash};

use rustc_hash::FxHasher;

use self::elems::{Elems, FindResult, Id};

/// This exists as a separate module to hide implementation details like:
///
/// - `Elems` stores elements as a `Vec`
/// - `Elem` IDs are determined by their index in the `Vec`
/// - [`Id`] and `Rank` happen to be integers
pub mod elems {
   use std::cell::Cell;
   use std::collections::HashSet;
   use std::fmt::Debug;
   use std::ops::{Add, Index};

   #[cfg(not(feature = "compact"))]
   type UfPtrType = usize;

   #[cfg(feature = "compact")]
   type UfPtrType = u32;

   #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
   struct UfPtr(UfPtrType);

   impl UfPtr {
      const MAX: UfPtr = UfPtr(UfPtrType::MAX);

      #[inline]
      fn from_usize(s: usize) -> Self {
         debug_assert!(s < Self::MAX.to_usize());
         UfPtr(s as UfPtrType)
      }

      #[inline]
      fn to_usize(&self) -> usize {
         debug_assert!(usize::try_from(self.0).is_ok());
         self.0 as usize
      }
   }

   impl Add<UfPtrType> for UfPtr {
      type Output = UfPtr;

      fn add(self, rhs: UfPtrType) -> Self::Output { UfPtr(self.0 + rhs) }
   }

   #[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
   pub struct Id(UfPtr);

   #[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
   pub(super) struct Rank(UfPtr);

   impl Add<UfPtrType> for Rank {
      type Output = Rank;

      fn add(self, rhs: UfPtrType) -> Self::Output { Rank(self.0 + rhs) }
   }

   /// Each field is wrapped in a [`Cell`] so that the union-find can expose
   /// a pure interface that uses mutation internally, that is to say,
   /// operations like [`UnionFind::find`] can internally mutate an element's
   /// parent/rank/next pointer.
   #[derive(Clone, Debug, Eq, PartialEq)]
   pub(super) struct Elem<T: PartialEq> {
      /// Circular linked list of equivalence class in the union-find
      ///
      /// This is initialized to the element's own [`Id`].
      pub(super) next: Cell<Id>,
      /// Parent in the union-find
      ///
      /// This is initialized to the element's own [`Id`].
      pub(super) parent: Cell<Id>,
      /// Rank in the union-find
      ///
      /// This is initialized to zero.
      rank: Cell<Rank>,
      /// Value
      pub(super) value: T,
   }

   impl<T: PartialEq> Elem<T> {
      /// In-place union
      pub fn union(&self, other: &Elem<T>) {
         // NB: At this point, both nodes should be roots, so their parent IDs
         // are their own IDs
         debug_assert_ne!(self.parent.get(), other.parent.get());
         debug_assert!(self.rank >= other.rank);
         let self_next = self.next.replace(other.next.get());
         self.rank.set(self.rank.get() + 1);
         other.next.set(self_next);
         other.parent.set(self.parent.get());
         debug_assert!(self.rank >= other.rank);
      }

      /// In-place union by rank
      pub(super) fn union_by_rank(&self, other: &Elem<T>) -> Id {
         // NB: At this point, both nodes should be roots, so their parent IDs
         // are their own IDs
         debug_assert_ne!(self.parent.get(), other.parent.get());
         let self_rank = self.rank.get();
         let other_rank = other.rank.get();
         if self_rank >= other_rank {
            self.union(other);
            return self.parent.get();
         } else {
            other.union(self);
            return other.parent.get();
         };
      }
   }

   #[derive(Debug)]
   pub(super) struct FindResult<'a, T: PartialEq> {
      pub id: Id,
      pub elem: &'a Elem<T>,
   }

   /// A collection of elements.
   ///
   /// Internally, an element's [`Id`] is determined by its position in the
   /// [`Vec`].
   #[derive(Clone, Debug, Default, Eq, PartialEq)]
   pub(super) struct Elems<T: PartialEq>(Vec<Elem<T>>);

   impl<T: PartialEq> Elems<T> {
      /// SAFETY: The ID must already exist in the union-find.
      pub(super) unsafe fn find(&self, id: Id) -> FindResult<T> {
         debug_assert!(self.has(id));
         let elem = unsafe { self.get_unchecked(id) };
         let parent_id = elem.parent.get();
         if id == parent_id {
            return FindResult { id, elem };
         }
         let parent = unsafe { self.get_unchecked(parent_id) };
         let grandparent_id = parent.parent.get();
         if grandparent_id == parent_id {
            return FindResult { id: parent_id, elem: parent };
         }
         // Path halving
         elem.parent.set(grandparent_id);
         self.find(grandparent_id)
      }

      pub(super) fn get(&self, id: Id) -> Option<&Elem<T>> { self.0.get(id.0.to_usize()) }

      #[inline]
      pub(super) fn has(&self, id: Id) -> bool { id.0.to_usize() < self.0.len() }

      #[inline]
      pub(super) unsafe fn get_unchecked(&self, id: Id) -> &Elem<T> {
         debug_assert!(self.has(id));
         unsafe { self.0.get_unchecked(id.0.to_usize()) }
      }

      pub(super) unsafe fn _get_unchecked_mut(&mut self, id: Id) -> &mut Elem<T> {
         debug_assert!(self.has(id));
         self.0.get_unchecked_mut(id.0.to_usize())
      }

      /// Iterate over all of the elements
      pub(super) fn iter(&self) -> impl Iterator<Item = (Id, &Elem<T>)> {
         self.0.iter().enumerate().map(|(i, e)| (Id(UfPtr::from_usize(i)), e))
      }

      /// Returns an iterator over the class of the node with the given [`Id`].
      ///
      /// Doesn't include the node with the given ID.
      #[allow(dead_code)]
      pub(super) fn iter_class(&self, start: Id) -> Option<impl Iterator<Item = (Id, &Elem<T>)>> {
         Class::new(self, start)
      }

      /// Returns an iterator over the class of the node with the given [`Id`].
      ///
      /// Doesn't include the node with the given ID.
      ///
      /// SAFETY: The ID must exist.
      pub(super) unsafe fn iter_class_unchecked(&self, start: Id) -> impl Iterator<Item = (Id, &Elem<T>)> {
         Class::new_unchecked(self, start)
      }

      /// Iterate over the equivalence classes
      ///
      /// Guaranteed to yield the root of each class first in each iterator.
      ///
      /// This doesn't do anything super smart, it just iterates over *all*
      /// elements, keeping track of (and skipping) equivalence classes with
      /// roots that its already seen.
      #[allow(dead_code)]
      pub(super) fn iter_classes(&self) -> impl Iterator<Item = impl Iterator<Item = (Id, &Elem<T>)>> {
         Classes::new(self)
      }

      #[inline]
      pub(super) fn len(&self) -> usize { self.0.len() }

      #[inline]
      fn next(&self) -> Id { Id(UfPtr::from_usize(self.len())) }

      /// O(1)
      #[allow(dead_code)]
      pub(super) fn ok_cheap(&self) -> bool { self.len() < UfPtr::MAX.to_usize() }

      /// O(n^2)
      #[allow(dead_code)]
      pub(super) fn ok(&self) -> bool {
         let mut count = 0;
         for (id, current) in self.iter() {
            count += 1;

            if !self.has(current.next.get()) {
               // eprintln!("Bad next! {:?}", self);
               return false;
            }
            if !self.has(current.parent.get()) {
               // eprintln!("Bad parent! {:?}", self);
               return false;
            }
            if current.rank.get() > Rank(UfPtr::from_usize(self.len())) {
               // eprintln!("Bad rank! {:?}", self);
               return false;
            }

            // No cycles
            let mut prev = HashSet::new();
            let mut id = id;
            let mut n = current;
            while n.parent.get() != id {
               if prev.contains(&id) {
                  // eprintln!("Cycle! {:?}", self);
                  return false;
               }
               prev.insert(id);

               let parent = &self[n.parent.get()];
               if n.rank.get() > parent.rank.get() {
                  // eprintln!("Bad rank! {:#?}", self);
                  return false;
               }

               id = n.parent.get();
               n = parent;
            }

            let root = id;

            // Circular linked list
            for (distance, (node_id, _)) in unsafe { self.iter_class_unchecked(id) }.enumerate() {
               // Each element in the iterator is equivalent to current
               if root != unsafe { self.find(node_id) }.id {
                  // eprintln!("Bad root in equivalence class");
                  return false;
               }
               if distance == self.len() {
                  // eprintln!("Bad linked list! {:#?}", self);
                  return false;
               }
            }
         }

         let count_by_classes = self.iter_classes().map(|i| i.count()).sum();
         // eprintln!("Count: {count}");
         // eprintln!("Len: {}", self.len());
         // eprintln!("Classes: {count_by_classes}");
         self.ok_cheap() && count == self.len() && count == count_by_classes
      }

      #[inline]
      pub(super) fn _is_empty(&self) -> bool { self.0.is_empty() }

      pub(super) fn push(&mut self, value: T) -> Id {
         debug_assert!(self.ok_cheap());
         let id = self.next();
         self.0.push(Elem { next: Cell::new(id), parent: Cell::new(id), rank: Cell::new(Rank(UfPtr(0))), value });
         assert!(self.len() < UfPtr::MAX.to_usize());
         id
      }

      #[inline]
      pub(super) fn _with_capacity(cap: usize) -> Self { Self(Vec::with_capacity(cap)) }
   }

   impl<T: PartialEq> Index<Id> for Elems<T> {
      type Output = Elem<T>;

      #[inline]
      fn index(&self, index: Id) -> &Self::Output { self.get(index).unwrap() }
   }

   /// Iterator over a single equivalence class
   struct Class<'a, T: PartialEq> {
      elems: &'a Elems<T>,
      start: Id,
      current: &'a Elem<T>,
   }

   impl<'a, T: PartialEq> Class<'a, T> {
      fn new(elems: &'a Elems<T>, start: Id) -> Option<Self> {
         let current = elems.get(start)?;
         Some(Class { elems, start, current })
      }

      unsafe fn new_unchecked(elems: &'a Elems<T>, start: Id) -> Self {
         Class { elems, start, current: elems.get_unchecked(start) }
      }
   }

   impl<'a, T: PartialEq> Iterator for Class<'a, T> {
      type Item = (Id, &'a Elem<T>);

      fn next(&mut self) -> Option<Self::Item> {
         let next_id = self.current.next.get();
         if next_id == self.start {
            None
         } else {
            // SAFETY: This should be fine; this ID was produced by the UF or
            // was explicitly unchecked in the constructor.
            self.current = unsafe { self.elems.get_unchecked(next_id) };
            Some((next_id, self.current))
         }
      }
   }

   /// Iterate over all the classes in a union-find
   struct Classes<'a, T: PartialEq> {
      elems: &'a Elems<T>,
      /// Root IDs already seen
      seen: HashSet<Id>,
      /// Inner iterator over elements of `Elem`
      iter: std::slice::Iter<'a, Elem<T>>,
   }

   impl<'a, T: PartialEq> Classes<'a, T> {
      fn new(elems: &'a Elems<T>) -> Self { Self { elems, seen: HashSet::new(), iter: elems.0.iter() } }
   }

   /// Guaranteed to yield the root of each class first in each iterator.
   impl<'a, T: PartialEq> Iterator for Classes<'a, T> {
      type Item = std::iter::Chain<std::iter::Once<(Id, &'a Elem<T>)>, Class<'a, T>>;

      fn next(&mut self) -> Option<Self::Item> {
         let mut next = self.iter.next()?;
         // SAFETY: This should be fine; this ID was produced by the UF.
         let mut root = unsafe { self.elems.find(next.parent.get()) };
         while self.seen.contains(&root.id) {
            next = self.iter.next()?;
            root = unsafe { self.elems.find(next.parent.get()) };
         }
         self.seen.insert(root.id);
         let first = std::iter::once((root.id, root.elem));
         let rest = unsafe { Class::new_unchecked(self.elems, root.id) };
         Some(first.chain(rest))
      }
   }
}

/// Invariant: `elems` and `items` have the same size.
///
/// Note: stores each element twice to support iteration.
#[derive(Debug, Default)]
pub struct UnionFind<T: Clone + Hash + Eq> {
   /// Elements are the equivalence-class structure.
   elems: Elems<T>,
   /// Items are the things being equated.
   ///
   /// Each item's [`Id`] is in a [`Cell`] so that it can be updated to point
   /// to the root of the equivalence class in [`UnionFind::find_item`].
   items: HashMap<T, Cell<Id>, BuildHasherDefault<FxHasher>>,
}

impl<T: Clone + Hash + Eq> UnionFind<T> {
   pub fn add(&mut self, item: T) -> (bool, Id) {
      debug_assert!(self.ok_cheap()); // invariant
      match self.find_item(&item) {
         Some(id) => (false, id),
         None => (true, self.push(item)),
      }
   }

   /// Version of [`Self::add`] that only clones the item if it wasn't present
   pub fn add_clone(&mut self, item: &T) -> (bool, Id) {
      debug_assert!(self.ok_cheap()); // invariant
      match self.find_item(item) {
         Some(id) => (false, id),
         None => (true, self.push(item.clone())),
      }
   }

   /// SAFETY: The ID must already exist in the union-find.
   pub unsafe fn find(&self, id: Id) -> Id {
      debug_assert!(self.elems.has(id));
      self.elems.find(id).id
   }

   pub fn find_item(&self, item: &T) -> Option<Id> { self.find_item_internal(item).map(|r| r.id) }

   fn find_item_internal(&self, item: &T) -> Option<FindResult<T>> {
      match self.items.get(item) {
         None => None,
         Some(id_cell) => {
            let id = id_cell.get();
            // SAFETY: This should be fine, since we know `id` is in the UF
            let fr @ FindResult { id: root_id, .. } = unsafe { self.elems.find(id) };
            id_cell.set(root_id);
            Some(fr)
         },
      }
   }

   #[inline]
   pub fn is_empty(&self) -> bool {
      debug_assert!(self.ok_cheap()); // invariant
      self.items.is_empty()
   }

   #[inline]
   pub fn len(&self) -> usize {
      debug_assert!(self.ok_cheap()); // invariant
      self.elems.len()
   }

   /// O(1)
   fn ok_cheap(&self) -> bool { self.elems.len() == self.items.len() }

   /// O(n^2)
   #[allow(dead_code)]
   fn ok(&self) -> bool { self.ok_cheap() && self.elems.ok() }

   fn push(&mut self, item: T) -> Id {
      debug_assert!(self.ok_cheap()); // invariant
      let id = self.elems.push(item.clone());
      self.items.insert(item, Cell::new(id));
      id
   }

   /// SAFETY: IDs must already exist in the union-find.
   unsafe fn union_internal(&self, x: Id, y: Id) -> FindResult<T> {
      debug_assert!(self.elems.has(x));
      debug_assert!(self.elems.has(y));

      if x == y {
         return self.elems.find(x);
      }
      let x_result = self.elems.find(x);
      let y_result = self.elems.find(y);
      if x_result.id == y_result.id {
         return x_result; // already in the same set
      }

      // Union by rank: The node with the greater rank becomes the root
      let root_id = x_result.elem.union_by_rank(y_result.elem);
      if root_id == x_result.id {
         x_result
      } else {
         debug_assert_eq!(root_id, y_result.id);
         y_result
      }
   }

   /// SAFETY: IDs must already exist in the union-find.
   pub unsafe fn union(&self, x: Id, y: Id) -> Id {
      debug_assert!(self.elems.has(x));
      debug_assert!(self.elems.has(y));
      self.union_internal(x, y).id
   }

   pub fn union_add(&mut self, x: T, y: T) -> Id {
      let (_, x_id) = self.add(x);
      let (_, y_id) = self.add(y);
      // SAFETY: We just added them.
      unsafe { self.union(x_id, y_id) }
   }

   pub fn union_add_clone(&mut self, x: &T, y: &T) -> Id {
      let (_, x_id) = self.add_clone(x);
      let (_, y_id) = self.add_clone(y);
      // SAFETY: We just added them.
      unsafe { self.union(x_id, y_id) }
   }
}

#[cfg(test)]
mod tests {
   use proptest::prelude::{Strategy, any};
   use proptest::proptest;

   use super::UnionFind;

   /// `u8`s to generate collisions
   #[derive(Copy, Clone, Debug)]
   enum Op {
      Add(u8),
      Find(u8),
      Union(u8, u8),
   }

   fn op_strat() -> impl Strategy<Value = Op> {
      proptest::prop_oneof![
         any::<u8>().prop_map(Op::Add),
         any::<u8>().prop_map(Op::Find),
         any::<(u8, u8)>().prop_map(|(a, b)| Op::Union(a, b)),
      ]
   }

   proptest! {

       #[test]
       fn uf_ok(ops in proptest::collection::vec(op_strat(), 1..100)) {

           // Perform a sequence of operations
           let mut uf = UnionFind::default();
           for (i, op) in ops.iter().enumerate() {
               assert!(uf.len() <= 2 * i); // union can add 2
               match *op {
                   Op::Add(x) => { uf.add(x); },
                   Op::Find(x) => { uf.find_item(&x); },
                   Op::Union(x, y) => { uf.union_add(x, y); },
               }
               assert!(uf.ok_cheap());
           }

           assert!(uf.ok()); // invariants

           for op in ops {
               match op {
                   // Not new
                   Op::Add(x) => assert!(!uf.add(x).0),
                   Op::Find(_) => (),
                   // Still in the same set
                   Op::Union(x, y) => assert_eq!(uf.find_item(&x), uf.find_item(&y)),
               }
               assert!(uf.ok_cheap());
           }
       }
   }
}
