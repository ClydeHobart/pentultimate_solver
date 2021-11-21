use super::{
	*,
	Addr as AddrTrait,
	Transformation as Trfm,
	FullAddr as Addr
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

impl<'a> LinePack<'a> {
	pub fn iter<F>(&self, mut f: F) -> ()
		where
			F: FnMut(usize, WordPack) -> ()
	{
		for word_index in 0_usize .. Library::WORD_COUNT {
			f(word_index, self.get_word_pack(word_index))
		}
	}
}

pub struct LinePackMut<'a> {
	pub trfm:	&'a mut Line<Trfm>,
	pub quat:	&'a mut Line<Quat>,
	pub mask:	&'a mut Line<Mask>,
	pub addr:	&'a mut Line<Addr>
}

impl<'a> LinePackMut<'a> {
	pub fn iter_mut<F>(&mut self, mut f: F) -> ()
		where
			F: FnMut(usize, WordPackMut) -> ()
	{
		for word_index in 0_usize .. Library::WORD_COUNT {
			f(word_index, self.get_word_pack_mut(word_index))
		}
	}
}

pub struct PagePack<'a> {
	pub trfm:	&'a Page<Trfm>,
	pub quat:	&'a Page<Quat>,
	pub mask:	&'a Page<Mask>,
	pub addr:	&'a Page<Addr>
}

impl<'a> PagePack<'a> {
	pub fn iter<F>(&self, mut f: F) -> ()
		where
			F: FnMut(usize, LinePack) -> ()
	{
		for line_index in 0_usize .. Library::LINE_COUNT {
			f(line_index, self.get_line_pack(line_index))
		}
	}
}

pub struct PagePackMut<'a> {
	pub trfm:	&'a mut Page<Trfm>,
	pub quat:	&'a mut Page<Quat>,
	pub mask:	&'a mut Page<Mask>,
	pub addr:	&'a mut Page<Addr>
}

impl<'a> PagePackMut<'a> {
	pub fn iter_mut<F>(&mut self, mut f: F) -> ()
		where
			F: FnMut(usize, LinePackMut) -> ()
	{
		for line_index in 0_usize .. Library::LINE_COUNT {
			f(line_index, self.get_line_pack_mut(line_index))
		}
	}
}

pub struct LongPagePack<'a> {
	pub trfm:	&'a LongPage<Trfm>,
	pub quat:	&'a LongPage<Quat>,
	pub mask:	&'a LongPage<Mask>,
	pub addr:	&'a LongPage<Addr>
}

impl<'a> LongPagePack<'a> {
	pub fn iter<F>(&self, mut f: F) -> ()
		where
			F: FnMut(usize, LinePack) -> ()
	{
		for line_index in 0_usize .. Library::LONG_LINE_COUNT {
			f(line_index, self.get_line_pack(line_index))
		}
	}
}

pub struct LongPagePackMut<'a> {
	pub trfm:	&'a mut LongPage<Trfm>,
	pub quat:	&'a mut LongPage<Quat>,
	pub mask:	&'a mut LongPage<Mask>,
	pub addr:	&'a mut LongPage<Addr>
}

impl<'a> LongPagePackMut<'a> {
	pub fn iter_mut<F>(&mut self, mut f: F) -> ()
		where
			F: FnMut(usize, LinePackMut) -> ()
	{
		for line_index in 0_usize .. Library::LONG_LINE_COUNT {
			f(line_index, self.get_line_pack_mut(line_index))
		}
	}
}

pub struct BookPack<'a> {
	pub trfm:	&'a Book<Trfm>,
	pub quat:	&'a Book<Quat>,
	pub mask:	&'a Book<Mask>,
	pub addr:	&'a Book<Addr>
}

impl<'a> BookPack<'a> {
	pub fn iter<'b, F>(&'b self, mut f: F) -> ()
		where
			F: FnMut(usize, PagePack<'b>) -> (),
			'a: 'b,
	{
		for page_index in 0_usize .. Library::PAGE_COUNT {
			f(page_index, self.get_page_pack(page_index))
		}
	}
}

pub struct BookPackMut<'a> {
	pub trfm:	&'a mut Book<Trfm>,
	pub quat:	&'a mut Book<Quat>,
	pub mask:	&'a mut Book<Mask>,
	pub addr:	&'a mut Book<Addr>
}

impl<'a> BookPackMut<'a> {
	pub fn iter_mut<'b, F>(&'b mut self, mut f: F) -> ()
		where
			F: FnMut(usize, PagePackMut) -> (),
			'a: 'b
	{
		for page_index in 0_usize .. Library::PAGE_COUNT {
			f(page_index, self.get_page_pack_mut(page_index))
		}
	}
}

pub struct BookPackData {
	pub trfm:	Book<Trfm>,
	pub quat:	Book<Quat>,
	pub mask:	Book<Mask>,
	pub addr:	Book<Addr>
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

impl<'a, 'b> GetWordPack<'b, (usize, usize)> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack(&'b self, (line_index, word_index): (usize, usize)) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfm[line_index][word_index],
			quat: &self.quat[line_index][word_index],
			mask: &self.mask[line_index][word_index],
			addr: &self.addr[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize)> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (line_index, word_index): (usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfm[line_index][word_index],
			quat: &mut self.quat[line_index][word_index],
			mask: &mut self.mask[line_index][word_index],
			addr: &mut self.addr[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, HalfAddr> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack(&'b self, addr: HalfAddr) -> WordPack<'b> {
		self.get_word_pack((addr.get_long_line_index(), addr.get_word_index()))
	}
}

impl<'a, 'b> GetWordPackMut<'b, HalfAddr> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, addr: HalfAddr) -> WordPackMut<'b> {
		self.get_word_pack_mut((addr.get_long_line_index(), addr.get_word_index()))
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

impl<'a, 'b> GetWordPack<'b, FullAddr> for BookPack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, address: FullAddr) -> WordPack<'b> {
		self.get_word_pack((address.get_page_index(), address.get_line_index(), address.get_word_index()))
	}
}

impl<'a, 'b> GetWordPackMut<'b, FullAddr> for BookPackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, address: FullAddr) -> WordPackMut<'b> {
		self.get_word_pack_mut((address.get_page_index(), address.get_line_index(), address.get_word_index()))
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize, usize)> for BookPackData where 'a: 'b {
	fn get_word_pack(&'b self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPack<'b> {
		WordPack {
			trfm: &self.trfm[page_index][line_index][word_index],
			quat: &self.quat[page_index][line_index][word_index],
			mask: &self.mask[page_index][line_index][word_index],
			addr: &self.addr[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize, usize)> for BookPackData where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			trfm: &mut self.trfm[page_index][line_index][word_index],
			quat: &mut self.quat[page_index][line_index][word_index],
			mask: &mut self.mask[page_index][line_index][word_index],
			addr: &mut self.addr[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, FullAddr> for BookPackData where 'a: 'b {
	fn get_word_pack(&'b self, address: FullAddr) -> WordPack<'b> {
		self.get_word_pack((address.get_page_index(), address.get_line_index(), address.get_word_index()))
	}
}

impl<'a, 'b> GetWordPackMut<'b, FullAddr> for BookPackData where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, address: FullAddr) -> WordPackMut<'b> {
		self.get_word_pack_mut((address.get_page_index(), address.get_line_index(), address.get_word_index()))
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

impl<'a, 'b> GetLinePack<'b, usize> for LongPagePack<'a> where 'a: 'b {
	fn get_line_pack(&'b self, line_index: usize) -> LinePack<'b> {
		LinePack {
			trfm: &self.trfm[line_index],
			quat: &self.quat[line_index],
			mask: &self.mask[line_index],
			addr: &self.addr[line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, usize> for LongPagePackMut<'a> where 'a: 'b {
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

impl<'a, 'b> GetLinePack<'b, (usize, usize)> for BookPackData where 'a: 'b {
	fn get_line_pack(&'b self, (page_index, line_index): (usize, usize)) -> LinePack<'b> {
		LinePack {
			trfm: &self.trfm[page_index][line_index],
			quat: &self.quat[page_index][line_index],
			mask: &self.mask[page_index][line_index],
			addr: &self.addr[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, (usize, usize)> for BookPackData where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, (page_index, line_index): (usize, usize)) -> LinePackMut<'b> {
		LinePackMut {
			trfm: &mut self.trfm[page_index][line_index],
			quat: &mut self.quat[page_index][line_index],
			mask: &mut self.mask[page_index][line_index],
			addr: &mut self.addr[page_index][line_index]
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

impl<'a, 'b> GetPagePack<'b, usize> for BookPackData where 'a: 'b {
	fn get_page_pack(&'b self, page_index: usize) -> PagePack<'b> {
		PagePack {
			trfm: &self.trfm[page_index],
			quat: &self.quat[page_index],
			mask: &self.mask[page_index],
			addr: &self.addr[page_index]
		}
	}
}

impl<'a, 'b> GetPagePackMut<'b, usize> for BookPackData where 'a: 'b {
	fn get_page_pack_mut(&'b mut self, page_index: usize) -> PagePackMut<'b> {
		PagePackMut {
			trfm: &mut self.trfm[page_index],
			quat: &mut self.quat[page_index],
			mask: &mut self.mask[page_index],
			addr: &mut self.addr[page_index]
		}
	}
}

pub trait GetBookPack<'b, T> {
	fn get_book_pack(&'b self, extra_params: T) -> BookPack<'b>;
}

pub trait GetBookPackMut<'b, T> {
	fn get_book_pack_mut(&'b mut self, extra_params: T) -> BookPackMut<'b>;
}

impl<'b> GetBookPack<'b, ()> for BookPackData {
	fn get_book_pack(&'b self, _: ()) -> BookPack<'b> {
		BookPack {
			trfm: &self.trfm,
			quat: &self.quat,
			mask: &self.mask,
			addr: &self.addr
		}
	}
}

impl<'b> GetBookPackMut<'b, ()> for BookPackData {
	fn get_book_pack_mut(&'b mut self, _: ()) -> BookPackMut<'b> {
		BookPackMut {
			trfm: &mut self.trfm,
			quat: &mut self.quat,
			mask: &mut self.mask,
			addr: &mut self.addr
		}
	}
}