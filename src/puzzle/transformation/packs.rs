use super::*;

pub struct WordPack<'a> {
	pub transformation:	&'a Word<Transformation>,
	pub mask:			&'a Word<Mask>,
	pub inverse:		&'a Word<FullAddr>,
}

pub struct WordPackMut<'a> {
	pub transformation:	&'a mut Word<Transformation>,
	pub mask:			&'a mut Word<Mask>,
	pub inverse:		&'a mut Word<FullAddr>,
}

pub struct LinePack<'a> {
	pub transformation:	&'a Line<Transformation>,
	pub mask:			&'a Line<Mask>,
	pub inverse:		&'a Line<FullAddr>,
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
	pub transformation:	&'a mut Line<Transformation>,
	pub mask:			&'a mut Line<Mask>,
	pub inverse:		&'a mut Line<FullAddr>,
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
	pub transformation:	&'a Page<Transformation>,
	pub mask:			&'a Page<Mask>,
	pub inverse:		&'a Page<FullAddr>,
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
	pub transformation:	&'a mut Page<Transformation>,
	pub mask:			&'a mut Page<Mask>,
	pub inverse:		&'a mut Page<FullAddr>,
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
	pub transformation:	&'a LongPage<Transformation>,
	pub mask:			&'a LongPage<Mask>,
	pub inverse:		&'a LongPage<FullAddr>,
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
	pub transformation:	&'a mut LongPage<Transformation>,
	pub mask:			&'a mut LongPage<Mask>,
	pub inverse:		&'a mut LongPage<FullAddr>,
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
	pub transformation:	&'a Book<Transformation>,
	pub mask:			&'a Book<Mask>,
	pub inverse:		&'a Book<FullAddr>,
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
	pub transformation:	&'a mut Book<Transformation>,
	pub mask:			&'a mut Book<Mask>,
	pub inverse:		&'a mut Book<FullAddr>,
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
	pub transformation:	Book<Transformation>,
	pub mask:			Book<Mask>,
	pub inverse:		Book<FullAddr>
}

pub trait GetWordPack<'b, T> {
	fn get_word_pack(&'b self, extra_params: T) -> WordPack<'b>;
}

pub trait GetWordPackMut<'b, T> {
	fn get_word_pack_mut(&'b mut self, extra_params: T) -> WordPackMut<'b>;
}

impl<'a, 'b> GetWordPack<'b, usize> for LinePack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, word_index: usize) -> WordPack<'b> {
		WordPack {
			transformation:	&self.transformation		[word_index],
			mask:			&self.mask					[word_index],
			inverse:		&self.inverse				[word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, usize> for LinePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, word_index: usize) -> WordPackMut<'b> {
		WordPackMut {
			transformation:	&mut self.transformation	[word_index],
			mask:			&mut self.mask				[word_index],
			inverse:		&mut self.inverse			[word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize)> for PagePack<'a> where 'a: 'b {
	fn get_word_pack(&'b self, (line_index, word_index): (usize, usize)) -> WordPack<'b> {
		WordPack {
			transformation:	&self.transformation		[line_index][word_index],
			mask:			&self.mask					[line_index][word_index],
			inverse:		&self.inverse				[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize)> for PagePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (line_index, word_index): (usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			transformation:	&mut self.transformation	[line_index][word_index],
			mask:			&mut self.mask				[line_index][word_index],
			inverse:		&mut self.inverse			[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPack<'b, (usize, usize)> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack(&'b self, (line_index, word_index): (usize, usize)) -> WordPack<'b> {
		WordPack {
			transformation:	&self.transformation		[line_index][word_index],
			mask:			&self.mask					[line_index][word_index],
			inverse:		&self.inverse				[line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize)> for LongPagePackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (line_index, word_index): (usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			transformation:	&mut self.transformation	[line_index][word_index],
			mask:			&mut self.mask				[line_index][word_index],
			inverse:		&mut self.inverse			[line_index][word_index]
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
			transformation:	&self.transformation		[page_index][line_index][word_index],
			mask:			&self.mask					[page_index][line_index][word_index],
			inverse:		&self.inverse				[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize, usize)> for BookPackMut<'a> where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			transformation:	&mut self.transformation	[page_index][line_index][word_index],
			mask:			&mut self.mask				[page_index][line_index][word_index],
			inverse:		&mut self.inverse			[page_index][line_index][word_index]
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
			transformation:	&self.transformation		[page_index][line_index][word_index],
			mask:			&self.mask					[page_index][line_index][word_index],
			inverse:		&self.inverse				[page_index][line_index][word_index]
		}
	}
}

impl<'a, 'b> GetWordPackMut<'b, (usize, usize, usize)> for BookPackData where 'a: 'b {
	fn get_word_pack_mut(&'b mut self, (page_index, line_index, word_index): (usize, usize, usize)) -> WordPackMut<'b> {
		WordPackMut {
			transformation:	&mut self.transformation	[page_index][line_index][word_index],
			mask:			&mut self.mask				[page_index][line_index][word_index],
			inverse:		&mut self.inverse			[page_index][line_index][word_index]
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
			transformation:	&self.transformation		[line_index],
			mask:			&self.mask					[line_index],
			inverse:		&self.inverse				[line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, usize> for PagePackMut<'a> where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, line_index: usize) -> LinePackMut<'b> {
		LinePackMut {
			transformation:	&mut self.transformation	[line_index],
			mask:			&mut self.mask				[line_index],
			inverse:		&mut self.inverse			[line_index]
		}
	}
}

impl<'a, 'b> GetLinePack<'b, usize> for LongPagePack<'a> where 'a: 'b {
	fn get_line_pack(&'b self, line_index: usize) -> LinePack<'b> {
		LinePack {
			transformation:	&self.transformation		[line_index],
			mask:			&self.mask					[line_index],
			inverse:		&self.inverse				[line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, usize> for LongPagePackMut<'a> where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, line_index: usize) -> LinePackMut<'b> {
		LinePackMut {
			transformation:	&mut self.transformation	[line_index],
			mask:			&mut self.mask				[line_index],
			inverse:		&mut self.inverse			[line_index]
		}
	}
}

impl<'a, 'b> GetLinePack<'b, (usize, usize)> for BookPack<'a> where 'a: 'b {
	fn get_line_pack(&'b self, (page_index, line_index): (usize, usize)) -> LinePack<'b> {
		LinePack {
			transformation:	&self.transformation		[page_index][line_index],
			mask:			&self.mask					[page_index][line_index],
			inverse:		&self.inverse				[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, (usize, usize)> for BookPackMut<'a> where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, (page_index, line_index): (usize, usize)) -> LinePackMut<'b> {
		LinePackMut {
			transformation:	&mut self.transformation	[page_index][line_index],
			mask:			&mut self.mask				[page_index][line_index],
			inverse:		&mut self.inverse			[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePack<'b, (usize, usize)> for BookPackData where 'a: 'b {
	fn get_line_pack(&'b self, (page_index, line_index): (usize, usize)) -> LinePack<'b> {
		LinePack {
			transformation:	&self.transformation		[page_index][line_index],
			mask:			&self.mask					[page_index][line_index],
			inverse:		&self.inverse				[page_index][line_index]
		}
	}
}

impl<'a, 'b> GetLinePackMut<'b, (usize, usize)> for BookPackData where 'a: 'b {
	fn get_line_pack_mut(&'b mut self, (page_index, line_index): (usize, usize)) -> LinePackMut<'b> {
		LinePackMut {
			transformation:	&mut self.transformation	[page_index][line_index],
			mask:			&mut self.mask				[page_index][line_index],
			inverse:		&mut self.inverse			[page_index][line_index]
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
			transformation:	&self.transformation		[page_index],
			mask:			&self.mask					[page_index],
			inverse:		&self.inverse				[page_index]
		}
	}
}

impl<'a, 'b> GetPagePackMut<'b, usize> for BookPackMut<'a> where 'a: 'b {
	fn get_page_pack_mut(&'b mut self, page_index: usize) -> PagePackMut<'b> {
		PagePackMut {
			transformation:	&mut self.transformation	[page_index],
			mask:			&mut self.mask				[page_index],
			inverse:		&mut self.inverse			[page_index]
		}
	}
}

impl<'a, 'b> GetPagePack<'b, usize> for BookPackData where 'a: 'b {
	fn get_page_pack(&'b self, page_index: usize) -> PagePack<'b> {
		PagePack {
			transformation:	&self.transformation		[page_index],
			mask:			&self.mask					[page_index],
			inverse:		&self.inverse				[page_index]
		}
	}
}

impl<'a, 'b> GetPagePackMut<'b, usize> for BookPackData where 'a: 'b {
	fn get_page_pack_mut(&'b mut self, page_index: usize) -> PagePackMut<'b> {
		PagePackMut {
			transformation:	&mut self.transformation	[page_index],
			mask:			&mut self.mask				[page_index],
			inverse:		&mut self.inverse			[page_index]
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
			transformation:	&self.transformation,
			mask:			&self.mask,
			inverse:		&self.inverse
		}
	}
}

impl<'b> GetBookPackMut<'b, ()> for BookPackData {
	fn get_book_pack_mut(&'b mut self, _: ()) -> BookPackMut<'b> {
		BookPackMut {
			transformation:	&mut self.transformation,
			mask:			&mut self.mask,
			inverse:		&mut self.inverse
		}
	}
}