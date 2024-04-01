/*
 * Copyright (c) Adrian Alic <contact@alic.dev>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

//! This module contains the aarch64 implementation of the lexer.
use std::arch::{aarch64::*, asm, global_asm};
use std::{iter::Peekable, str::Chars};

use super::common::{keyword_hash, Token, TokenKind, KEYWORD_MAP};

/* SIMD mapping for multi-sequence characters */
const SIMD_MAP_ALPHABETIC: u8 = TokenKind::Ident as u8;
const SIMD_MAP_NUMBER: u8/*.*/= TokenKind::Number as u8;
const SIMD_MAP_WHITESPACE: u8 = TokenKind::Whitespace as u8;
const SIMD_MAP_USCORE: u8 = 0x5f;

/// Size of the SIMD buffer in bytes. We process a large chunk of bytes of the
/// input string at a time, which is then read by the [next_token] function.
/// After reaching the end of such a chunk, we process the next chunk.
pub const SIMD_BUFFER_SIZE: u16 = 512;

#[repr(align(128))]
pub struct SimdBuffer([u8; SIMD_BUFFER_SIZE as usize]);

/// Lexer implementation (aarch64). Optimized with NEON SIMD instructions.
#[repr(C)]
pub struct Lexer<'a> {
    pub chars: Peekable<Chars<'a>>,
    text: &'a [u8],
    pos: u32,
    simd_idx: u16,
    simd_buffer: SimdBuffer,
    peeked: Option<Token>,
    current: Option<Token>,
    current_len: u32,
}

global_asm!(include_str!("aarch64.s"));

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut lexer = Lexer {
            chars: s.chars().peekable(),
            text: s.as_bytes(),
            pos: 0,
            simd_idx: 0,
            simd_buffer: SimdBuffer([0u8; SIMD_BUFFER_SIZE as usize]),
            peeked: None,
            current: None,
            current_len: 0,
        };
        if lexer.text.len() > SIMD_BUFFER_SIZE as usize {
            unsafe { lexer.simd_fill() };
        }
        lexer
    }
    #[inline]
    pub fn peek_token(&mut self) -> Option<&Token> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next_token().clone();
        self.peeked.as_ref()
    }
    #[inline]
    pub fn current_token(&mut self) -> Option<Token> {
        self.current.clone()
    }
    /// Returns a byte slice of the token.
    #[inline]
    pub fn slice(&mut self) -> &'a [u8] {
        let current_idx = self.pos as usize - SIMD_BUFFER_SIZE as usize
            + self.simd_idx as usize;
        &self.text[(current_idx - self.current_len as usize)..current_idx]
    }

    // Transposed lookup table for indexing 4 128-bit SIMD registers
    const fn lookup_table_uint8x16x4(from: u8) -> [u8; 64] {
        let register_count = 4;
        let mut x = [0u8; 64];
        // transpose
        let mut i = 0usize;
        while i < 64 {
            let offset = i / register_count;
            let idx = (i % register_count) * 16 + offset;
            x[i] = from + idx as u8;
            i += 1;
        }
        // transform
        let mut i = 0usize;
        while i < 64 {
            if x[i].is_ascii_alphabetic() {
                x[i] = SIMD_MAP_ALPHABETIC;
            } else if x[i].is_ascii_whitespace() {
                x[i] = SIMD_MAP_WHITESPACE;
            } else if x[i].is_ascii_digit() {
                x[i] = SIMD_MAP_NUMBER;
            }

            i += 1;
        }
        x
    }

    const CONV_TABLE1: &[u8; 64] = &Self::lookup_table_uint8x16x4(0);
    const CONV_TABLE2: &[u8; 64] = &Self::lookup_table_uint8x16x4(64);

    unsafe fn printx<T>(t: T) {
        let x: [u8; 16] = unsafe { *std::mem::transmute::<&T, &[u8; 16]>(&t) };
        for i in x.iter().rev() {
            print!("{:0>2x} ", i);
        }
        println!("");
    }

    /// Fill SIMD buffer, starting from `Lexer::pos` until the `pos + B` where
    /// `B` is the buffer size.
    #[inline(always)]
    unsafe fn simd_fill(&mut self) {
        // compile-time invariant
        const _: () = {
            assert!(SIMD_BUFFER_SIZE % 16 == 0, "should be multiple of 16");
        };

        let ptr: *const u8 = &self.text[self.pos as usize];

        // We invalidate the entire SIMD buffer, so the index can be reset.
        self.simd_idx = 0;

        let conv1: uint8x16x4_t = unsafe { vld4q_u8(&Self::CONV_TABLE1[0]) };
        let conv2: uint8x16x4_t = unsafe { vld4q_u8(&Self::CONV_TABLE2[0]) };

        // constant vector registers
        let v192 = vld1q_dup_u8(&0xc0);

        for i in 0..(SIMD_BUFFER_SIZE as usize / 16) {
            let offset = 16 * i;
            let vinput = vld1q_u8(ptr.add(offset));
            let v1_upper = vaddq_u8(vinput, v192); // v1[i] = v[i] - 64;
            let index_1 = vqtbl4q_u8(conv1, vinput);
            let index_2 = vqtbl4q_u8(conv2, v1_upper);
            let masked = vreinterpretq_u16_u8(vorrq_u8(index_1, index_2));

            vst1q_u16(
                self.simd_buffer.0.as_mut_ptr().add(offset) as *mut _,
                masked,
            );
        }

        self.pos += SIMD_BUFFER_SIZE as u32;
    }

    #[inline(never)]
    pub fn asm_exp(&mut self) -> u32 {
        Self::print_register(24);
        13
    }

    #[inline(never)]
    fn print_register(x: u32) {
        println!("{x}");
    }

    #[inline(always)]
    pub fn next_token(&mut self) -> Option<Token> {
        self.current_len = 1;

        // Read next token. This one is the actual enum value we will return
        let ret = self.simd_buffer.0[self.simd_idx as usize];

        // EOF
        if std::intrinsics::unlikely(ret == 0) {
            return None;
        }

        let token = Some(Token {
            kind: unsafe { std::mem::transmute(ret) },
        });

        // if no MSB set, return the single-character token immediately
        if ret < 0x80 {
            _ = self.incr_simd_idx(1);
            return token;
        }

        /* * * * * * * * * * * *
         *  Skip Loop Section  *
         * * * * * * * * * * * */

        let start = (self.pos - SIMD_BUFFER_SIZE as u32 + self.simd_idx as u32)
            as usize;

        self.incr_simd_idx(1)?;
        let mut next1 = self.simd_buffer.0[self.simd_idx as usize];

        // skip-loop for multi-character sequences
        // FIXME: Allow identifiers longer than 64
        loop {
            if !Self::skip_condition(ret, next1) {
                break;
            }
            self.current_len += 1;
            self.incr_simd_idx(1)?;
            next1 = self.simd_buffer.0[self.simd_idx as usize];
        }

        // identifier keyword matching
        if ret == SIMD_MAP_ALPHABETIC && self.current_len < 8 {
            static BITMASKS: [u64; 8] = [
                0x0000000000000000,
                0x00000000000000ff,
                0x000000000000ffff,
                0x0000000000ffffff,
                0x00000000ffffffff,
                0x000000ffffffffff,
                0x0000ffffffffffff,
                0x00ffffffffffffff,
            ];
            let candidate = unsafe {
                let p = self.text.get_unchecked(start) as *const _;
                (p as *const u64).read_unaligned()
            };
            let candidate = candidate & BITMASKS[self.current_len as usize];
            let candidate: [u8; 8] = candidate.to_le_bytes();
            let (hash, keyword) = keyword_hash(&candidate);
            if std::intrinsics::unlikely(candidate == KEYWORD_MAP[hash]) {
                return Some(Token {
                    kind: keyword.unwrap(),
                });
            }
        }

        token
    }

    #[inline(always)]
    fn skip_condition(tip: u8, next: u8) -> bool {
        tip == next
            || (tip == SIMD_MAP_ALPHABETIC
                && (next == SIMD_MAP_USCORE || next == SIMD_MAP_NUMBER))
    }

    /// Increment SIMD buffer index, and fill buffer on overflow.
    /// Returns none if the simd_fill would spill over the input.
    #[inline(always)]
    #[must_use]
    fn incr_simd_idx(&mut self, count: u16) -> Option<()> {
        self.simd_idx = (self.simd_idx + count) & (SIMD_BUFFER_SIZE - 1);
        if self.simd_idx == 0 {
            if self.pos as usize >= self.text.len() {
                return None;
            }
            unsafe { self.simd_fill() };
        }
        Some(())
    }
}
