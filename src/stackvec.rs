/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::mem::MaybeUninit;

#[derive(Debug)]
pub enum Error {
    CapacityReached,
}
pub struct StackVec<T, const SIZE: usize> {
    data: [MaybeUninit<T>; SIZE],
    len: usize,
}

impl<T, const SIZE: usize> StackVec<T, SIZE> {
    pub fn push(&mut self, t: T) -> Result<(), Error> {
        if self.len >= SIZE - 1 {
            return Err(Error::CapacityReached);
        }
        self.data[self.len].write(t);
        self.len += 1;
        Ok(())
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn as_slice(&self) -> &[T] {
        unsafe { core::mem::transmute(&self.data[0..self.len]) }
    }
}
impl<T, const SIZE: usize> Default for StackVec<T, SIZE> {
    fn default() -> Self {
        Self {
            data: unsafe { MaybeUninit::uninit().assume_init() },
            len: 0,
        }
    }
}
