use super::{
	*,
	Transformation as Trfm,
	Addr as Addr
};
pub struct WordPack<'a> {
	pub trfm:	&'a Word<Trfm>,
	pub quat:	&'a Word<Quat>,
	pub mask:	&'a Word<Mask>,
	pub addr:	&'a Word<Addr>
}

pub struct WordPackMut<'a> {
	pub trfm:	&'a mut Word<Trfm>,
	pub quat:	&'a mut Word<Quat>,
	pub mask:	&'a mut Word<Mask>,
	pub addr:	&'a mut Word<Addr>
}

pub struct LinePack<'a> {
	pub trfm:	&'a Line<Trfm>,
	pub quat:	&'a Line<Quat>,
	pub mask:	&'a Line<Mask>,
	pub addr:	&'a Line<Addr>
}

pub struct LinePackMut<'a> {
	pub trfm:	&'a mut Line<Trfm>,
	pub quat:	&'a mut Line<Quat>,
	pub mask:	&'a mut Line<Mask>,
	pub addr:	&'a mut Line<Addr>
}

pub struct PagePack<'a> {
	pub trfm:	&'a Page<Trfm>,
	pub quat:	&'a Page<Quat>,
	pub mask:	&'a Page<Mask>,
	pub addr:	&'a Page<Addr>
}

pub struct PagePackMut<'a> {
	pub trfm:	&'a mut Page<Trfm>,
	pub quat:	&'a mut Page<Quat>,
	pub mask:	&'a mut Page<Mask>,
	pub addr:	&'a mut Page<Addr>
}

pub struct BookPack<'a> {
	pub trfm:	&'a Book<Trfm>,
	pub quat:	&'a Book<Quat>,
	pub mask:	&'a Book<Mask>,
	pub addr:	&'a Book<Addr>
}

pub struct BookPackMut<'a> {
	pub trfm:	&'a mut Book<Trfm>,
	pub quat:	&'a mut Book<Quat>,
	pub mask:	&'a mut Book<Mask>,
	pub addr:	&'a mut Book<Addr>
}

pub trait GetWordPack<'b, T> {
	fn get_word_pack(&'b self, extra_params: T) -> WordPack<'b>;
}

pub trait GetWordPackMut<'b, T> {
	fn get_word_pack_mut(&'b mut self, extra_params: T) -> WordPackMut<'b>;
}

impl<'a, 'b> GetWordPack<'b, usize> for LinePack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, rotation_index: usize) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfm[rotation_index],
			quat: &self.quat[rotation_index],
			mask: &self.mask[rotation_index],
			addr: &self.addr[rotation_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, usize> for LinePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, rotation_index: usize) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfm[rotation_index],
			quat: &mut self.quat[rotation_index],
			mask: &mut self.mask[rotation_index],
			addr: &mut self.addr[rotation_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize)> for PagePack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, (line_index, word_index): (usize, usize)) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfm[line_index][word_index],
			quat: &self.quat[line_index][word_index],
			mask: &self.mask[line_index][word_index],
			addr: &self.addr[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize)> for PagePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (line_index, word_index): (usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfm[line_index][word_index],
			quat: &mut self.quat[line_index][word_index],
			mask: &mut self.mask[line_index][word_index],
			addr: &mut self.addr[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize, usize)> for BookPack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfm[page_index][line_index][word_index],
			quat: &self.quat[page_index][line_index][word_index],
			mask: &self.mask[page_index][line_index][word_index],
			addr: &self.addr[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize, usize)> for BookPackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfm[page_index][line_index][word_index],
			quat: &mut self.quat[page_index][line_index][word_index],
			mask: &mut self.mask[page_index][line_index][word_index],
			addr: &mut self.addr[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, Addr> for BookPack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, address: Addr) -> WordPack<'b> {
		self.get_word_pack((address.page_index as usize, address.line_index as usize, address.word_index as usize))
	}
}

impl<'a, 'b> GetWordPackMut<'b, Addr> for BookPackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, address: Addr) -> WordPackMut<'b> {
		self.get_word_pack_mut((address.page_index as usize, address.line_index as usize, address.word_index as usize))
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize, usize)> for Library where 'a: 'b {
	fn get_word_pack(&'b self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfms[page_index][line_index][word_index],
			quat: &self.quats[page_index][line_index][word_index],
			mask: &self.masks[page_index][line_index][word_index],
			addr: &self.addrs[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize, usize)> for Library where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfms[page_index][line_index][word_index],
			quat: &mut self.quats[page_index][line_index][word_index],
			mask: &mut self.masks[page_index][line_index][word_index],
			addr: &mut self.addrs[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, Addr> for Library where 'a: 'b {
	fn get_word_pack(&'b self, address: Addr) -> WordPack<'b> {
		self.get_word_pack((address.page_index as usize, address.line_index as usize, address.word_index as usize))
	}
}

impl<'a, 'b> GetWordPackMut<'b, Addr> for Library where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, address: Addr) -> WordPackMut<'b> {
		self.get_word_pack_mut((address.page_index as usize, address.line_index as usize, address.word_index as usize))
	}
}

pub trait GetLinePack<'b, T> {
	fn get_line_pack(&'b self, extra_params: T) -> LinePack<'b>;
}

pub trait GetLinePackMut<'b, T> {
	fn get_line_pack_mut(&'b mut self, extra_params: T) -> LinePackMut<'b>;
}

impl<'a, 'b> GetLinePack<'b, usize> for PagePack<'a> where 'a: 'b {
	fn get_line_pack(&'b self, line_index: usize) -> LinePack<'b> {
		LinePack {
			trfm: &self.trfm[line_index],
			quat: &self.quat[line_index],
			mask: &self.mask[line_index],
			addr: &self.addr[line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, usize> for PagePackMut<'a> where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, line_index: usize) -> LinePackMut<'b> {
		LinePackMut {
			trfm: &mut self.trfm[line_index],
			quat: &mut self.quat[line_index],
			mask: &mut self.mask[line_index],
			addr: &mut self.addr[line_index]
		}
	}
}

impl<'a, 'b> GetLinePack<'b, (usize, usize)> for BookPack<'a> where 'a: 'b {
	fn get_line_pack(&'b self, (page_index, line_index): (usize, usize)) -> LinePack<'b> {
		LinePack {
			trfm: &self.trfm[page_index][line_index],
			quat: &self.quat[page_index][line_index],
			mask: &self.mask[page_index][line_index],
			addr: &self.addr[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, (usize, usize)> for BookPackMut<'a> where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, (page_index, line_index): (usize, usize)) -> LinePackMut<'b> {
		LinePackMut {
			trfm: &mut self.trfm[page_index][line_index],
			quat: &mut self.quat[page_index][line_index],
			mask: &mut self.mask[page_index][line_index],
			addr: &mut self.addr[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePack<'b, (usize, usize)> for Library where 'a: 'b {
	fn get_line_pack(&'b self, (page_index, line_index): (usize, usize)) -> LinePack<'b> {
		LinePack {
			trfm: &self.trfms[page_index][line_index],
			quat: &self.quats[page_index][line_index],
			mask: &self.masks[page_index][line_index],
			addr: &self.addrs[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, (usize, usize)> for Library where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, (page_index, line_index): (usize, usize)) -> LinePackMut<'b> {
		LinePackMut {
			trfm: &mut self.trfms[page_index][line_index],
			quat: &mut self.quats[page_index][line_index],
			mask: &mut self.masks[page_index][line_index],
			addr: &mut self.addrs[page_index][line_index]
		}
	}
}

pub trait GetPagePack<'b, T> {
	fn get_page_pack(&'b self, extra_params: T) -> PagePack<'b>;
}

pub trait GetPagePackMut<'b, T> {
	fn get_page_pack_mut(&'b mut self, extra_params: T) -> PagePackMut<'b>;
}

impl<'a, 'b> GetPagePack<'b, usize> for BookPack<'a> where 'a: 'b {
	fn get_page_pack(&'b self, page_index: usize) -> PagePack<'b> {
		PagePack {
			trfm: &self.trfm[page_index],
			quat: &self.quat[page_index],
			mask: &self.mask[page_index],
			addr: &self.addr[page_index]
		}
	}
}

impl<'a, 'b> GetPagePackMut<'b, usize> for BookPackMut<'a> where 'a: 'b {
	fn get_page_pack_mut(&'b mut self, page_index: usize) -> PagePackMut<'b> {
		PagePackMut {
			trfm: &mut self.trfm[page_index],
			quat: &mut self.quat[page_index],
			mask: &mut self.mask[page_index],
			addr: &mut self.addr[page_index]
		}
	}
}

impl<'a, 'b> GetPagePack<'b, usize> for Library where 'a: 'b {
	fn get_page_pack(&'b self, page_index: usize) -> PagePack<'b> {
		PagePack {
			trfm: &self.trfms[page_index],
			quat: &self.quats[page_index],
			mask: &self.masks[page_index],
			addr: &self.addrs[page_index]
		}
	}
}

impl<'a, 'b> GetPagePackMut<'b, usize> for Library where 'a: 'b {
	fn get_page_pack_mut(&'b mut self, page_index: usize) -> PagePackMut<'b> {
		PagePackMut {
			trfm: &mut self.trfms[page_index],
			quat: &mut self.quats[page_index],
			mask: &mut self.masks[page_index],
			addr: &mut self.addrs[page_index]
		}
	}
}

pub trait GetBookPack<'b, T> {
	fn get_book_pack(&'b self, extra_params: T) -> BookPack<'b>;
}

pub trait GetBookPackMut<'b, T> {
	fn get_book_pack_mut(&'b mut self, extra_params: T) -> BookPackMut<'b>;
}

impl<'b> GetBookPack<'b, ()> for Library {
	fn get_book_pack(&'b self, _: ()) -> BookPack<'b> {
		BookPack {
			trfm: &self.trfms,
			quat: &self.quats,
			mask: &self.masks,
			addr: &self.addrs
		}
	}
}

impl<'b> GetBookPackMut<'b, ()> for Library {
	fn get_book_pack_mut(&'b mut self, _: ()) -> BookPackMut<'b> {
		BookPackMut {
			trfm: &mut self.trfms,
			quat: &mut self.quats,
			mask: &mut self.masks,
			addr: &mut self.addrs
		}
	}
}

// I would like it to be known that I, Zeke Baker, absolutely *despise* Rust's lifetime system.
// It is cumbersome, convoluted, and the borrow checker isn't smart enough to see the soundness of many complex borrowing situations.

#[macro_export]
macro_rules! page_pack_iter {
	($book_pack:ident) => {
		(0_usize .. Library::PAGE_COUNT)
		.map(|page_index: usize| -> PagePack {
			$book_pack.get_page_pack(page_index)
		});
	};
}

#[macro_export]
macro_rules! line_pack_iter {
	($page_pack:ident) => {
		(0_usize .. Library::LINE_COUNT)
		.map(|line_index: usize| -> LinePack {
			$page_pack.get_line_pack(line_index)
		});
	};
}

#[macro_export]
macro_rules! word_pack_iter {
	($line_pack:ident) => {
		(0_usize .. Library::WORD_COUNT)
		.map(|word_index: usize| -> WordPack {
			$line_pack.get_word_pack(word_index)
		});
	};
}

#[macro_export]
macro_rules! page_pack_iter_mut {
	($book_pack_mut:ident) => {
		(0_usize .. Library::PAGE_COUNT)
		.map(|page_index: usize| -> PagePackMut {
			$book_pack_mut.get_page_pack_mut(page_index)
		});
	};
}

#[macro_export]
macro_rules! line_pack_iter_mut {
	($page_pack_mut:ident) => {
		(0_usize .. Library::LINE_COUNT)
		.map(|line_index: usize| -> LinePackMut {
			$page_pack_mut.get_line_pack_mut(line_index)
		});
	};
}

#[macro_export]
macro_rules! word_pack_iter_mut {
	($line_pack_mut:ident) => {
		(0_usize .. Library::WORD_COUNT)
		.map(|word_index: usize| -> WordPackMut {
			$line_pack_mut.get_word_pack_mut(word_index)
		});
	};
}