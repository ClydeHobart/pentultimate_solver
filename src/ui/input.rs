use {
	std::{
		collections::{
			HashMap,
			VecDeque
		},
		convert::TryFrom,
		fmt::{
			Debug,
			Formatter,
			Result as FmtResult,
			Write
		},
		mem::{
			take,
			transmute
		},
		ops::{
			DerefMut,
			Index
		},
		path::Path,
		rc::Rc,
		sync::{
			atomic::{
				AtomicBool,
				Ordering
			},
			Arc,
			Mutex
		},
		task::{
			Context as TaskContext,
			Poll,
			Wake,
			Waker
		},
		time::{
			Duration,
			Instant
		}
	},
	bevy::{
		app::CoreStage,
		input::{
			keyboard::KeyCode,
			mouse::{
				MouseMotion,
				MouseWheel
			}
		},
		prelude::*,
		utils::BoxedFuture
	},
	bevy_egui::{
		EguiContext as BevyEguiContext,
		EguiSystem
	},
	bevy_inspector_egui::{
		egui,
		Context,
		Inspectable
	},
	bit_field::BitField,
	egui::{
		CollapsingHeader,
		Color32,
		ComboBox,
		Context as EguiContext,
		Grid,
		Ui
	},
	rand::{
		rngs::ThreadRng,
		Rng,
		thread_rng
	},
	rfd::FileHandle,
	serde::{
		Deserialize,
		Deserializer,
		Serialize
	},
	crate::{
		app::{
			prelude::*,
			SaveState
		},
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		preferences::{
			AnimationSpeedData,
			RandomizationType,
			SpeedData
		},
		prelude::*,
		puzzle::{
			consts::*,
			transformation::{
				Action,
				Addr,
				FullAddr,
				GenusIndex,
				GenusIndexType,
				HalfAddr,
				Library,
				FullMask,
				RandHalfAddrParams
			},
			ExtendedPuzzleState,
			InflatedPuzzleState,
			InflatedPieceStateComponent as PSC
		}
	},
	super::{
		Preferences,
		View
	}
};

pub fn generate_default_positions() -> [usize; HALF_PENTAGON_PIECE_COUNT] {
	let mut positions: [usize; HALF_PENTAGON_PIECE_COUNT] = [0_usize; HALF_PENTAGON_PIECE_COUNT];
	let icosidodecahedron_data: &Data = Data::get(Polyhedron::Icosidodecahedron);
	let face_0_data: &FaceData = &icosidodecahedron_data.faces[0_usize];
	let face_1_norm: Vec3 = icosidodecahedron_data.faces[1_usize].norm;

	for rotation in 3_u32 ..= 7_u32 {
		positions[rotation as usize - 2] = icosidodecahedron_data
			.get_closest_face_index(
				&face_0_data
					.get_rotation_quat(rotation % PSC::PENTAGON_VERTEX_COUNT as u32)
					.mul_vec3(face_1_norm), 
				None
			);
	}

	positions
}

trait IntoKeyCode: Sized {
	fn into_key_code(self) -> KeyCode;
}

impl IntoKeyCode for u32 {
	fn into_key_code(self) -> KeyCode  { unsafe { transmute::<Self, KeyCode>(self) } }
}

#[test]
fn key_code_count() -> () {
	println!("{}", KeyCode::Cut as u32 + 1_u32);
}

#[derive(Clone, Copy)]
#[cfg_attr(not(target_os = "macos"), derive(Debug))]
#[repr(u8)]
enum Modifier {
	Alt,
	Ctrl,
	Shift,
	Win
}

#[cfg(target_os = "macos")]
impl Debug for Modifier {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		match self {
			Self::Alt => {
				f.write_str("Opt")
			},
			Self::Ctrl => {
				f.write_str("Ctrl")
			},
			Self::Shift => {
				f.write_str("Shift")
			},
			Self::Win => {
				f.write_str("Cmd")
			}
		}
	}
}

impl Modifier {
	const COUNT:		usize = 4_usize;
	const OFFSET:		usize = 8_usize;
	const LEFT:			char = '<';
	const RIGHT:		char = '>';
	const ALT_CHAR:		char = '!';
	const CTRL_CHAR:	char = '^';
	const SHIFT_CHAR:	char = '+';
	const WIN_CHAR:		char = '#';
	const KEY_CODES:	[KeyCode; Modifier::COUNT << 1] = [
		KeyCode::LAlt,
		KeyCode::LControl,
		KeyCode::LShift,
		KeyCode::LWin,
		KeyCode::RAlt,
		KeyCode::RControl,
		KeyCode::RShift,
		KeyCode::RWin
	];

	const fn full_mask(self) -> u16 {
		const FULL_MASKS: [u16; Modifier::COUNT] = Modifier::masks(true, true);

		FULL_MASKS[self as usize]
	}

	const fn left_key_code(self) -> KeyCode {
		Modifier::KEY_CODES[self as usize]
	}

	const fn left_mask(self) -> u16 {
		const LEFT_MASKS: [u16; Modifier::COUNT] = Modifier::masks(true, false);

		LEFT_MASKS[self as usize]
	}

	const fn right_mask(self) -> u16 {
		const RIGHT_MASK: [u16; Modifier::COUNT] = Modifier::masks(false, true);

		RIGHT_MASK[self as usize]
	}

	const fn right_key_code(self) -> KeyCode {
		Modifier::KEY_CODES[self as usize + Modifier::COUNT]
	}

	const fn char(self) -> char {
		const CHARS: [char; Modifier::COUNT] = [
			Modifier::WIN_CHAR,
			Modifier::ALT_CHAR,
			Modifier::CTRL_CHAR,
			Modifier::SHIFT_CHAR
		];

		CHARS[self as usize]
	}

	const fn masks(left: bool, right: bool) -> [u16; Modifier::COUNT] {
		let mut masks: [u16; Modifier::COUNT] = [0_u16; Modifier::COUNT];
		let mut modifier: usize = 0_usize;

		while modifier < Modifier::COUNT {
			masks[modifier] = Modifier::mask(modifier, left, right);
			modifier += 1_usize;
		}

		masks
	}

	const fn mask(modifier: usize, left: bool, right: bool) -> u16 {
		let offset: usize = (modifier << 1_usize) + Self::OFFSET;
		let mut mask: u16 = 0_u16;

		if left {
			mask |= 1_u16 << offset;
		}

		if right {
			mask |= 1_u16 << offset + 1_usize;
		}

		mask
	}

	fn is_modifier(key_code: KeyCode) -> bool {
		Self::KEY_CODES.binary_search(&key_code).is_ok()
	}
}

#[cfg(test)]
#[test]
fn verify_sorted_key_codes() -> () {
	for key_code_index in 1_usize .. Modifier::KEY_CODES.len() {
		assert!(Modifier::KEY_CODES[key_code_index - 1_usize] < Modifier::KEY_CODES[key_code_index]);
	}
}

impl From<usize> for Modifier { fn from(modifier: usize) -> Self { unsafe { transmute::<u8, Self>(modifier as u8) } } }

impl TryFrom<char> for Modifier {
	type Error = ();

	fn try_from(c: char) -> Result<Self, ()> {
		match c {
			Self::WIN_CHAR => Ok(Self::Win),
			Self::ALT_CHAR => Ok(Self::Alt),
			Self::CTRL_CHAR => Ok(Self::Ctrl),
			Self::SHIFT_CHAR => Ok(Self::Shift),
			_ => Err(())
		}
	}
}

#[derive(Clone, Copy, Deserialize, PartialEq, Serialize)]
#[serde(into = "String", try_from = "&str")]
pub struct KeyPress(u16);

impl KeyPress {
	const KEY_CODE_COUNT: u32 = 163_u32;
	const KEY_CODE_MASK: u16 = (1_u16 << Modifier::OFFSET) - 1_u16;
	const INVALID: Self = Self(Self::KEY_CODE_MASK);

	fn is_active(self, keyboard_input: &Input<KeyCode>) -> bool {
		self.is_valid()
			&& (0_usize .. Modifier::COUNT).all(|modifier: usize| -> bool {
					let modifier: Modifier = modifier.into();

					self.0 & modifier.full_mask() == 0_u16
						|| self.0 & modifier.left_mask() != 0_u16
						&& keyboard_input.pressed(modifier.left_key_code())
						|| self.0 & modifier.right_mask() != 0_u16
						&& keyboard_input.pressed(modifier.right_key_code())
				})
			&& keyboard_input.just_pressed(self.get_key_code())
	}

	#[inline(always)]
	fn is_valid(self) -> bool { self != Self::INVALID }

	#[inline]
	fn get_key_code(self) -> KeyCode {
		assert!(self.is_valid());

		((self.0 & Self::KEY_CODE_MASK) as u32).into_key_code()
	}

	#[inline]
	fn set_key_code(&mut self, key_code: KeyCode) -> () {
		self.0 = self.0 & !Self::KEY_CODE_MASK | key_code as u16;
	}

	#[inline]
	fn set_modifiers(&mut self, modifiers: u8) -> () {
		self.0 = self.0 & Self::KEY_CODE_MASK | (modifiers as u16) << Modifier::COUNT;
	}

	fn clashes(self, other: KeyPress) -> bool {
		if self.get_key_code() == other.get_key_code() {
			let mut self_specific_modifier_exists: bool = false;
			let mut other_specific_modifier_exists: bool = false;

			for modifier in 0_usize .. Modifier::COUNT {
				let full_mask: u16 = Modifier::from(modifier).full_mask();
				let self_full_mask: u16 = self.0 & full_mask;
				let other_full_mask: u16 = other.0 & full_mask;
				let self_uses_modifier: bool = self_full_mask != 0_u16;
				let other_uses_modifier: bool = other_full_mask != 0_u16;
				
				if self_full_mask ^ other_full_mask != 0_u16
					&& (self_full_mask != full_mask || !other_uses_modifier)
					&& (other_full_mask != full_mask || !self_uses_modifier)
				{
					self_specific_modifier_exists |= self_uses_modifier;
					other_specific_modifier_exists |= other_uses_modifier;

					if self_specific_modifier_exists && other_specific_modifier_exists {
						return false;
					}
				}
			}

			true
		} else {
			false
		}
	}
}

impl Debug for KeyPress {
	fn fmt(&self, f: &mut Formatter) -> FmtResult {
		for modifier in 0_usize .. Modifier::COUNT {
			let modifier: Modifier = modifier.into();

			if self.0 & modifier.full_mask() != 0_u16 {
				if self.0 & modifier.right_mask() == 0_u16 {
					f.write_str("L")?;
				} else if self.0 & modifier.left_mask() == 0_u16 {
					f.write_str("R")?;
				}

				write!(f, "{:?} + ", modifier)?;
			}
		}

		write!(f, "{:?}", self.get_key_code())
	}
}

impl Default for KeyPress { fn default() -> Self { Self::INVALID } }

impl From<(u8, KeyCode)> for KeyPress {
	fn from((modifiers, key_code): (u8, KeyCode)) -> Self {
		let mut key_press: KeyPress = KeyPress::default();

		key_press.set_modifiers(modifiers);
		key_press.set_key_code(key_code);

		key_press
	}
}

impl From<KeyPress> for String {
	fn from(key_press: KeyPress) -> Self {
		let mut string: String = String::new();

		for modifier in 0_usize .. Modifier::COUNT {
			let modifier: Modifier = modifier.into();

			if key_press.0 & modifier.full_mask() != 0_u16 {
				if key_press.0 & modifier.right_mask() == 0_u16 {
					write!(&mut string, "{}", Modifier::LEFT).unwrap();
				} else if key_press.0 & modifier.left_mask() == 0_u16 {
					write!(&mut string, "{}", Modifier::RIGHT).unwrap();
				}

				write!(&mut string, "{}", modifier.char()).unwrap();
			}
		}

		write!(
			&mut string,
			"{:?}",
			unsafe { transmute::<u32, KeyCode>((key_press.0 & KeyPress::KEY_CODE_MASK) as u32) }
		).unwrap();

		string
	}
}

impl Inspectable for KeyPress {
	type Attributes = Option<Rc<dyn Fn(KeyPress) -> Result<(), String>>>;

	fn ui(&mut self, ui: &mut Ui, is_valid: Self::Attributes, context: &mut Context) -> bool {
		let mut changed: bool = false;

		CollapsingHeader::new(format!("{:?}", self))
			.id_source(context.id())
			.show(ui, |ui: &mut Ui| -> () {
				const MIN_COL_WIDTH_SCALE: f32 = 2.0_f32;
				let col_width: f32 = MIN_COL_WIDTH_SCALE * ui.spacing().interact_size.x;

				Grid::new(0_u64)
					.num_columns(1_usize)
					.min_col_width(2.0_f32 * col_width)
					.show(ui, |ui: &mut Ui| -> () {
						let mut self_key_code: KeyCode = self.get_key_code();

						ui.centered_and_justified(|ui: &mut Ui| -> () {
							ComboBox::from_id_source(0_u64)
								.width(2.0_f32 * col_width)
								.selected_text(format!("{:?}", self_key_code))
								.show_ui(ui, |ui: &mut Ui| -> () {
									for key_code in 0_u32 .. Self::KEY_CODE_COUNT {
										let key_code: KeyCode = key_code.into_key_code();

										if !Modifier::is_modifier(key_code)
											&& ui.selectable_label(
												key_code == self_key_code,
												format!("{:?}", key_code)
											).clicked()
										{
											self.set_key_code(key_code);
											self_key_code = key_code;
											changed = true;
										}
									}
								});
						});

						ui.end_row();

						Grid::new(1_u64)
							.num_columns(2_usize)
							.min_col_width(col_width)
							.show(ui, |ui: &mut Ui| -> () {
								for modifier in 0_usize .. Modifier::COUNT {
									let modifier: Modifier = modifier.into();
									let mut button = |
										key_press: &mut KeyPress,
										ui: &mut Ui,
										mask: &dyn Fn(Modifier) -> u16,
										key_code: &dyn Fn(Modifier) -> KeyCode
									| -> () {
										ui.centered_and_justified(|ui: &mut Ui| -> () {
											let mask: u16 = mask(modifier);

											ui.visuals_mut().override_text_color = Some(if key_press.0 & mask != 0_u16 {
												ui.visuals().text_color()
											} else {
												ui.visuals().weak_text_color()
											});

											if ui.button(format!("{:?}", key_code(modifier))).clicked() {
												key_press.0 ^= mask;
												changed = true;
											}
										});
									};

									button(self, ui, &Modifier::left_mask, &Modifier::left_key_code);
									button(self, ui, &Modifier::right_mask, &Modifier::right_key_code);
									ui.end_row();
								}
							});

						ui.end_row();

						if let Err(string) = is_valid.map(|
							is_valid: Rc<dyn Fn(KeyPress) -> Result<(), String>>
						| -> Result<(), String> {
							is_valid(*self)
						}).unwrap_or(Ok(())) {
							ui.colored_label(Color32::RED, string);
							ui.end_row();
						}
					});
			});

		changed
	}
}

impl TryFrom<&str> for KeyPress {
	type Error = String;

	fn try_from(str_slice: &str) -> Result<Self, String> {
		struct LeftRight {
			left: bool,
			right: bool
		}

		let mut key_press: Self = Self(0_u16);
		let mut curr_byte: usize = 0_usize;
		let mut left_right: LeftRight = LeftRight {
			left: true,
			right: true
		};

		for c in str_slice.chars() {
			if let Ok(modifier) = Modifier::try_from(c) {
				if !left_right.left && !left_right.right {
					return Err(format!("Modifier {:?} has both left and right keys disabled", modifier));
				}

				key_press.0 |= Modifier::mask(modifier as usize, left_right.left, left_right.right);
				left_right = LeftRight {
					left: true,
					right: true
				};
			} else if c == Modifier::LEFT {
				left_right.right = false;
			} else if c == Modifier::RIGHT {
				left_right.left = false;
			} else {
				break;
			}

			curr_byte += c.len_utf8();
		}

		if let Some(key_code_str) = str_slice.get(curr_byte .. str_slice.len()) {
			match ron::de::from_str::<KeyCode>(&key_code_str) {
				Ok(key_code) => {
					key_press.set_key_code(key_code);

					Ok(key_press)
				},
				Err(err) => {
					Err(format!("{:?}", err))
				}
			}
		} else {
			Err(format!("couldn't grab slice starting at byte {}", curr_byte))
		}
	}
}

impl TryFrom<KeyCode> for KeyPress {
	type Error = ();

	fn try_from(key_code: KeyCode) -> Result<Self, ()> {
		if Modifier::is_modifier(key_code) {
			Err(())
		} else {
			let mut key_press: KeyPress = KeyPress(0_u16);

			key_press.set_key_code(key_code);

			Ok(key_press)
		}
	}
}

macro_rules! key_presses {
	($($action:ident: $key_press:literal),*) => {
		#[derive(Debug, Deserialize, Eq, Hash, PartialEq)]
		#[repr(u8)]
		pub enum KeyPressAction {
			$($action,)*
		}

		pub const KEY_PRESS_ACTION_COUNT: usize = {
			let mut count: usize = 0_usize;

			$(
				crate::ignore!($action);
				count += 1_usize;
			)*

			count
		};

		impl Default for KeyPresses {
			fn default() -> Self { Self::from({
				const KEY_PRESS_STR_ARRAY: [&'static str; KEY_PRESS_ACTION_COUNT] = [
					$(
						$key_press,
					)*
				];

				&KEY_PRESS_STR_ARRAY
			}) }
		}
	}
}

key_presses!(
	Rot0:						"Numpad0",
	Rot1:						"Numpad1",
	Rot2:						"Numpad4",
	Rot3:						"Numpad5",
	Rot4:						"Numpad6",
	Rot5:						"Numpad3",
	RecenterCamera:				"Space",
	Undo:						"^Z",
	Redo:						"^Y",
	CycleFamilyUp:				"Up",
	CycleFamilyDown:			"Down",
	CycleGenusUpWithinFamily:	"Right",
	CycleGenusDownWithinFamily:	"Left",
	EnableModifiers:			"E",
	RotateTwice:				"D",
	CounterClockwise:			"S",
	AltHemi:					"A",
	DisableRecentering:			"X"
);

impl KeyPressAction {
	fn from_usize(action: usize) -> Self {
		assert!(action < KEY_PRESS_ACTION_COUNT);

		unsafe { transmute::<u8, Self>(action as u8) }
	}
}

pub type ActionsBitField = u32;

const_assert!(KEY_PRESS_ACTION_COUNT <= ActionsBitField::BITS as usize);

#[derive(Clone, PartialEq)]
pub struct KeyCodeWithActions {
	key_code:	KeyCode,
	actions:	ActionsBitField
}

impl KeyCodeWithActions {
	fn key_code(&self) -> KeyCode { self.key_code }
}

#[derive(Clone, PartialEq)]
pub struct KeyPresses {
	action_to_key_press: [KeyPress; KEY_PRESS_ACTION_COUNT],
	key_code_to_actions: Vec<KeyCodeWithActions>
}

impl KeyPresses {
	fn try_get_key_code_index(&self, key_code: &KeyCode) -> Result<usize, usize> {
		self.key_code_to_actions.binary_search_by_key(key_code, KeyCodeWithActions::key_code)
	}

	fn update_key_press_for_action(
		&mut self,
		action_index:	usize,
		key_press:	KeyPress
	) -> () {
		let mut_key_press: &mut KeyPress = &mut self.action_to_key_press[action_index];
		let old_key_code: KeyCode = mut_key_press.get_key_code();
		let new_key_code: KeyCode = key_press.get_key_code();

		*mut_key_press = key_press;

		if old_key_code != new_key_code {
			self.remove_action_from_key_code(action_index, old_key_code);
			self.add_action_to_key_code(action_index, new_key_code);
		}
	}

	fn add_action_to_key_code(&mut self, action_index: usize, key_code: KeyCode) -> () {
		match self.try_get_key_code_index(&key_code) {
			Ok(key_code_index) => {
				self.key_code_to_actions[key_code_index].actions.set_bit(action_index, true);
			},
			Err(key_code_index) => {
				self.key_code_to_actions.insert(
					key_code_index,
					KeyCodeWithActions {
						key_code,
						actions: (1 as ActionsBitField) << action_index
					}
				)
			}
		}
	}

	fn remove_action_from_key_code(&mut self, action_index: usize, key_code: KeyCode) -> () {
		if let Ok(key_code_index) = self.try_get_key_code_index(&key_code) {
			let actions: &mut ActionsBitField = &mut self.key_code_to_actions[key_code_index].actions;
	
			actions.set_bit(action_index, false);
	
			if *actions == 0 as ActionsBitField {
				self.key_code_to_actions.remove(key_code_index);
			}
		}
	}
}

impl<'de> Deserialize<'de> for KeyPresses {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		let hash_map: HashMap<KeyPressAction, KeyPress> =
			HashMap::<KeyPressAction, KeyPress>::deserialize(deserializer)?;
		let mut key_presses: KeyPresses = KeyPresses::default();

		for action_index in 0_usize .. KEY_PRESS_ACTION_COUNT {
			if let Some(key_press) = hash_map.get(&KeyPressAction::from_usize(action_index)) {
				key_presses.update_key_press_for_action(action_index, *key_press);
			}
		}

		Ok(key_presses)
	}
}

impl From<&[&str; KEY_PRESS_ACTION_COUNT]> for KeyPresses {
	fn from(key_press_str_array: &[&str; KEY_PRESS_ACTION_COUNT]) -> Self {
		let mut key_presses: Self = Self {
			action_to_key_press: <[KeyPress; KEY_PRESS_ACTION_COUNT]>::default(),
			key_code_to_actions: Vec::<KeyCodeWithActions>::with_capacity(KEY_PRESS_ACTION_COUNT)
		};

		for (action_index, key_press_str) in key_press_str_array.iter().enumerate() {
			let key_press: KeyPress = KeyPress::try_from(*key_press_str).unwrap();

			key_presses.action_to_key_press[action_index] = key_press;
			key_presses.add_action_to_key_code(action_index, key_press.get_key_code());
		}

		key_presses
	}
}

impl<'a> From<&'a Rc<*mut KeyPresses>> for &'a mut KeyPresses {
	fn from(rc_self: &Rc<*mut KeyPresses>) -> Self { unsafe { (*rc_self).as_mut() }.unwrap() }
}

impl Index<KeyPressAction> for KeyPresses {
	type Output = KeyPress;

	fn index(&self, action: KeyPressAction) -> &KeyPress { &self.action_to_key_press[action as usize]}
}

impl Inspectable for KeyPresses {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
		let mut action_to_key_press: [KeyPress; KEY_PRESS_ACTION_COUNT] = self.action_to_key_press.clone();
		let rc_self: Rc<*mut Self> = Rc::<*mut Self>::new(self);
		let mut changed: bool = false;

		Grid::new(self as *const Self as usize).show(ui, |ui: &mut Ui| -> () {
			for (action_index, key_press) in action_to_key_press.iter_mut().enumerate() {
				let closure_rc_self: Rc<*mut Self> = rc_self.clone();
	
				ui.label(format!("{:?}", KeyPressAction::from_usize(action_index)));

				if key_press.ui(
					ui,
					Some(Rc::new(move |key_press: KeyPress| -> Result<(), String> {
						let key_code: KeyCode = key_press.get_key_code();
						let key_presses: &mut KeyPresses = (&closure_rc_self).into();

						if let Ok(key_code_index) = key_presses.try_get_key_code_index(&key_code) {
							let mut actions: ActionsBitField = key_presses
								.key_code_to_actions
								[key_code_index]
								.actions;
							let mut clashing_action_index: usize = actions.trailing_zeros() as usize;

							while clashing_action_index < KEY_PRESS_ACTION_COUNT {
								let clashing_key_press: KeyPress = key_presses
									.action_to_key_press
									[clashing_action_index];
								if clashing_action_index != action_index && clashing_key_press.clashes(key_press) {
									return Err(format!(
										"Clashes with {:?}: {:?}",
										KeyPressAction::from_usize(clashing_action_index),
										clashing_key_press
									));
								}

								actions.set_bit(clashing_action_index, false);
								clashing_action_index = actions.trailing_zeros() as usize;
							}
						}

						Ok(())
					})),
					&mut context.with_id(action_index as u64)
				) {
					changed = true;

					<&mut KeyPresses>::from(&rc_self).update_key_press_for_action(action_index, *key_press);
				}

				ui.end_row();
			}
		});

		changed
	}
}

define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub struct InputData {
		#[inspectable(collapse, max = Some(PENTAGON_PIECE_COUNT - 1_usize))]
		pub default_positions:		[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		#[inspectable(collapse)]
		pub key_presses:			KeyPresses								= KeyPresses::default()
	}
);

#[derive(Clone, Copy, Debug)]
pub enum PuzzleActionType {
	RecenterCamera,
	Transformation,
	Undo,
	Redo,
	Reset,
	Randomize,
	Load,
	Solve
}

pub struct CurrentAction {
	pub camera_orientation:		Quat,
	start:						Instant,
	duration:					Duration,
	pub action:					Action,
	pub has_camera_orientation:	bool
}

impl CurrentAction {
	pub fn s_now(&self) -> Option<f32> {
		if self.duration.is_zero() {
			None
		} else {
			let s: f32 = (Instant::now() - self.start).as_millis() as f32 / self.duration.as_millis() as f32;

			if s < 1.0_f32 {
				Some(s)
			} else {
				None
			}
		}
	}
}

#[derive(Default)]
pub struct PendingActions {
	pub puzzle_state:			InflatedPuzzleState,
	pub camera_orientation:		Quat,
	pub actions:				VecDeque<Action>,
	pub animation_speed_data:	AnimationSpeedData,
	pub curr_action:			usize,
	pub set_puzzle_state:		bool,
	pub set_camera_orientation:	bool,
	pub set_action_stack:		bool,
	pub set_curr_action:		bool
}

impl PendingActions {
	fn pop_current_action(
		&mut self,
		camera_query: &mut CameraQueryMut,
		action_type: PuzzleActionType
	) -> Option<CurrentAction> {
		if let (
			Some(action),
			Some(camera_orientation)
		) = (
			self.actions.pop_front(),
			CameraQueryNTMut(camera_query)
				.orientation(|camera_orientation: Option<&mut Quat>| -> Option<Quat> { camera_orientation.copied() })
		) {
			Some(CurrentAction {
				camera_orientation,
				start:					Instant::now(),
				duration:				action.compute_duration(&self.animation_speed_data, action_type),
				action,
				has_camera_orientation:	true,
			})
		} else {
			None
		}
	}

	pub fn reset(camera_orientation: &Quat, animation_speed_data: AnimationSpeedData) -> Self {
		Self {
			camera_orientation:		(*CameraPlugin::compute_camera_addr(camera_orientation)
				.as_reorientation()
				.get_inverse_addr()
				.get_rotation()
				.unwrap()
			) * (*camera_orientation),
			animation_speed_data,
			set_puzzle_state:		true,
			set_camera_orientation:	true,
			.. Self::default()
		}
	}

	pub fn randomize(preferences: &Preferences, puzzle_state: &InflatedPuzzleState, camera_orientation: &Quat) -> Self {
		let mut pending_actions: Self = Self {
			animation_speed_data: preferences.speed.animation.clone(),
			.. Self::default()
		};
		let (puzzle_state, actions): (InflatedPuzzleState, VecDeque<Action>) = {
			let random_transformation_count: usize = preferences
				.file_menu
				.randomization_params
				.random_transformation_count
				as usize;
			let genus_indices: Vec<GenusIndex> = {
				let mut genus_indices: Vec<GenusIndex> = Vec::<GenusIndex>::new();

				for genus_index_usize in 0_usize .. Library::get_genus_count() {
					if preferences.file_menu.randomization_params.random_transformation_genera.get_bit(genus_index_usize) {
						genus_indices.push(GenusIndex::try_from(genus_index_usize as GenusIndexType).unwrap());
					}
				}

				genus_indices
			};
			let genus_index_count: f32 = genus_indices.len() as f32;
			let mut puzzle_state: InflatedPuzzleState = if matches!(
				preferences.file_menu.randomization_params.randomization_type,
				RandomizationType::FromCurrent
			) { puzzle_state.clone() } else { InflatedPuzzleState::SOLVED_STATE };
			let mut camera: HalfAddr = CameraPlugin::compute_camera_addr(camera_orientation);
			let mut actions: VecDeque<Action> = VecDeque::<Action>::with_capacity(random_transformation_count);
			let mut thread_rng: ThreadRng = thread_rng();

			while actions.len() < random_transformation_count {
				let transformation: FullAddr = FullAddr::from((
					genus_indices[(thread_rng.gen::<f32>() * genus_index_count) as usize],
					HalfAddr::from(RandHalfAddrParams { thread_rng: &mut thread_rng, allow_species_large_indices: false })
				));

				if transformation.is_identity_transformation() {
					continue;
				}

				if let Some(prev_action) = actions.back() {
					let prev_transformation: FullAddr = prev_action.transformation;

					if match (
						transformation.try_get_genus_index().unwrap(),
						prev_transformation.try_get_genus_index().unwrap()
					) {
						(
							GenusIndex::REORIENTATION,
							GenusIndex::REORIENTATION
						) => true,
						(
							GenusIndex::SIMPLE,
							GenusIndex::SIMPLE
						) => transformation.get_species_index() == prev_transformation.get_species_index(),
						_ => transformation.get_inverse_addr() == prev_transformation
					} {
						continue;
					}
				}

				let action: Action = Action::new(transformation, camera);
				let standardization: HalfAddr = action.standardization();

				actions.push_back(action);
				puzzle_state += transformation;
				puzzle_state += standardization;
				camera = action.get_camera_end() + standardization;
			}

			(puzzle_state, actions)
		};

		match preferences.file_menu.randomization_params.randomization_type {
			RandomizationType::FromCurrent => {
				pending_actions.actions = actions;
			},
			RandomizationType::FromSolved => {
				pending_actions.actions = actions;
				pending_actions.set_puzzle_state = true;
			},
			RandomizationType::FromSolvedNoStack => {
				pending_actions.puzzle_state = puzzle_state;
				pending_actions.set_puzzle_state = true;
			}
		};

		pending_actions
	}

	pub fn load(save_state: &SaveState, animation_speed_data: &AnimationSpeedData) -> Self { Self {
		puzzle_state:			save_state.extended_puzzle_state.puzzle_state.clone(),
		camera_orientation:		save_state.camera.get_orientation().copied().unwrap_or_default(),
		actions:				save_state.extended_puzzle_state.actions.clone().into(),
		animation_speed_data:	animation_speed_data.clone(),
		curr_action:			save_state.extended_puzzle_state.curr_action,
		set_puzzle_state:		true,
		set_camera_orientation:	true,
		set_action_stack:		true,
		set_curr_action:		true
	} }
}

pub struct PuzzleAction {
	pub current_action:		Option<CurrentAction>,
	pub pending_actions:	Option<Box<PendingActions>>,
	pub action_type:		PuzzleActionType
}

impl PuzzleAction {
	/// # `update()`
	///
	/// ## Return value
	///
	/// `bool` representing whether or not the action has been completed
	pub fn update(
		&mut self,
		extended_puzzle_state:	&mut ExtendedPuzzleState,
		queries:				&mut QuerySet<(
			CameraQueryStateMut,
			PieceQueryState
		)>
	) -> bool {
		let mut completed: bool = true;

		loop {
			if self.current_action.is_none() {
				if let Some(pending_actions) = self
					.pending_actions
					.as_mut()
					.map(Box::<PendingActions>::as_mut)
				{
					if pending_actions.set_puzzle_state {
						*extended_puzzle_state = ExtendedPuzzleState {
							puzzle_state: take(&mut pending_actions.puzzle_state).into(),
							.. ExtendedPuzzleState::default()
						};
						extended_puzzle_state.puzzle_state.update_pieces(&mut queries.q1());
						pending_actions.set_puzzle_state = false;
					}

					if pending_actions.set_camera_orientation {
						CameraQueryNTMut(&mut queries.q0()).orientation(|camera_orientation: Option<&mut Quat>| -> () {
							if let Some(camera_orientation) = camera_orientation {
								*camera_orientation = pending_actions.camera_orientation;
							}
						});
						pending_actions.set_camera_orientation = false;
					}

					if pending_actions.set_action_stack {
						extended_puzzle_state.actions = take(&mut pending_actions.actions).into();
						pending_actions.set_action_stack = false;
					}

					if pending_actions.set_curr_action {
						extended_puzzle_state.curr_action = pending_actions.curr_action;
						pending_actions.set_curr_action = false;
					}

					self.current_action = pending_actions
						.pop_current_action(&mut queries.q0(), self.action_type);
				}
			}

			if let Some(current_action) = &self.current_action {
				let action: Action = current_action.action;
				let end_quat: Option<Quat> = action.get_camera_end().get_orientation().copied();

				match current_action.s_now() {
					Some(s) => {
						if action.transformation.is_valid() {
							let comprising_simples: &[HalfAddr] = action
								.transformation
								.get_simple_slice();
							let mut cycle_count: f32 = 0.0_f32;
							let mut puzzle_state: InflatedPuzzleState = extended_puzzle_state.puzzle_state.clone();
							
							let s: f32 = s * FullAddr::get_simple_slice_cycles(
								&comprising_simples
							) as f32;

							for comprising_simple in comprising_simples {
								let comprising_simple: FullAddr = comprising_simple.as_simple();
								let cycles: f32 = comprising_simple.get_cycles() as f32;

								if cycles == 0.0_f32 {
									continue;
								} else if s >= cycle_count + cycles {
									puzzle_state += comprising_simple;
									cycle_count += cycles;
								} else {
									let mask: FullMask = *comprising_simple.get_full_mask().unwrap();
									let rotation: Quat = Quat::IDENTITY.short_slerp(
										*comprising_simple.get_rotation().unwrap(),
										(s - cycle_count) / cycles
									);

									for (piece_component, mut transform) in queries
										.q1()
										.iter_mut()
									{
										let piece_index: usize = piece_component.index;

										transform.rotation = if mask
											.affects_piece(puzzle_state.pos[piece_index] as usize)
										{
											rotation
										} else {
											Quat::IDENTITY
										} * (*puzzle_state.half_addr(piece_index).get_orientation().unwrap());
									}

									break;
								}
							}
						}

						if current_action.has_camera_orientation {
							CameraQueryNTMut(&mut queries.q0())
								.orientation(|camera_orientation: Option<&mut Quat>| -> () {
									if let (
										Some(camera_orientation),
										Some(end_quat)
									) = (
										camera_orientation,
										end_quat
									) {
										*camera_orientation = current_action
											.camera_orientation
											.short_slerp(end_quat, s);
									}
								});
						}

						completed = false;

						break;
					},
					None => {
						let mut standardization_quat: Option<Quat> = None;

						if action.transformation.is_valid() {
							let standardization_addr: FullAddr = action.standardization().as_reorientation();

							extended_puzzle_state.puzzle_state += action.transformation;
							extended_puzzle_state.puzzle_state += standardization_addr;
							warn_expect!(extended_puzzle_state.puzzle_state.is_standardized());

							extended_puzzle_state.puzzle_state.update_pieces(&mut queries.q1());

							if !action.transformation.is_genus_index_reorientation() {
								standardization_quat = standardization_addr.get_rotation().copied();
							}
						}

						if action.camera_start.is_valid() {
							CameraQueryNTMut(&mut queries.q0())
								.orientation(|camera_orientation: Option<&mut Quat>| -> () {
									if let Some(camera_orientation) = camera_orientation {
										*camera_orientation = standardization_quat.unwrap_or_default()
										* if current_action.has_camera_orientation && end_quat.is_some() {
											end_quat.unwrap()
										} else {
											*camera_orientation
										};
									}
								});
						}

						match self.action_type {
							PuzzleActionType::Transformation
								| PuzzleActionType::Randomize
								| PuzzleActionType::Solve =>
							{
								if warn_expect!(action.transformation.is_valid()) {
									extended_puzzle_state.actions.truncate(extended_puzzle_state.curr_action);
									extended_puzzle_state.actions.push(action);
									extended_puzzle_state.curr_action += 1_usize;
								}
							},
							PuzzleActionType::Undo => {
								extended_puzzle_state.curr_action -= 1_usize;
							},
							PuzzleActionType::Redo => {
								extended_puzzle_state.curr_action += 1_usize;
							},
							_ => {}
						}

						self.current_action = None;
					}
				}
			} else {
				break;
			}
		}

		completed
	}
}

pub enum FileActionType {
	Save,
	Load
}

pub struct Ready(AtomicBool);

impl Wake for Ready {
	fn wake(self: Arc<Self>) -> () { self.0.store(true, Ordering::Relaxed); }
}

pub struct FileAction {
	future:			Mutex<BoxedFuture<'static, Option<FileHandle>>>,
	ready:			Arc<Ready>,
	action_type:	FileActionType
}

impl FileAction {
	pub fn new(future: Mutex<BoxedFuture<'static, Option<FileHandle>>>, action_type: FileActionType) -> Self { Self {
		future,
		ready: Arc::<Ready>::new(Ready(true.into())),
		action_type
	} }

	fn poll(&mut self) -> Option<Option<FileHandle>> {
		if self.check_ready() {
			let waker: Waker = self.ready.clone().into();
			let mut context: TaskContext = TaskContext::from_waker(&waker);

			if let Poll::Ready(file_handle) = self
				.future
				.get_mut()
				.unwrap()
				.as_mut()
				.poll(&mut context)
			{
				Some(file_handle)
			} else {
				None
			}
		} else {
			None
		}
	}

	fn check_ready(&mut self) -> bool { self.ready.0.swap(false, Ordering::Relaxed) }
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub struct InputToggles {
	pub enable_modifiers:		bool,
	pub rotate_twice:			bool,
	pub counter_clockwise:		bool,
	pub alt_hemi:				bool,
	pub disable_recentering:	bool,
	pub genus_index:			GenusIndex,
}

impl InputToggles {
	fn half_addr(&self, default_position: usize) -> HalfAddr {
		HalfAddr::new(self.line_index(default_position), self.word_index())
	}

	fn line_index(&self, default_position: usize) -> usize {
		if self.alt_hemi {
			InflatedPuzzleState::invert_position(default_position)
		} else {
			default_position
		}
	}

	fn word_index(&self) -> usize {
		if self.enable_modifiers {
			(
				1_i32
					* if self.rotate_twice { 2_i32 } else { 1_i32 }
					* if self.counter_clockwise { -1_i32 } else { 1_i32 }
					+ PSC::PENTAGON_VERTEX_COUNT as i32
			)
				as usize
				% usize::PENTAGON_VERTEX_COUNT
		} else {
			0_usize
		}
	}

	fn update(&mut self, key_presses: &KeyPresses, keyboard_input: &Input<KeyCode>) -> () {
		use KeyPressAction as KPA;

		macro_rules! update_toggle {
			($($toggle:ident: $action:ident),*) => {
				$(
					if key_presses[KPA::$action].is_active(keyboard_input) {
						self.$toggle = !self.$toggle;
					}
				)*
			}
		}

		update_toggle!(
			enable_modifiers:		EnableModifiers,
			rotate_twice:			RotateTwice,
			counter_clockwise:		CounterClockwise,
			alt_hemi:				AltHemi,
			disable_recentering:	DisableRecentering
		);

		if let Some(should_increment) = if key_presses
			[KPA::CycleFamilyDown]
			.is_active(keyboard_input)
		{
			Some(true)
		} else if key_presses[KPA::CycleFamilyUp].is_active(keyboard_input) {
			Some(false)
		} else {
			None
		} {
			let family_count: usize = Library::get_family_count();

			self.genus_index = Library::get_base_genus_index(
				(Library::get_family_index(self.genus_index)
					+ if should_increment { 1_usize } else { family_count - 1_usize }
				) % family_count
			);
		} else if let Some(should_increment) = if key_presses
			[KPA::CycleGenusUpWithinFamily]
			.is_active(keyboard_input)
		{
			Some(true)
		} else if key_presses[KPA::CycleGenusDownWithinFamily].is_active(keyboard_input) {
			Some(false)
		} else {
			None
		} {
			use GenusIndexType as GIT;

			let family_index: usize = Library::get_family_index(self.genus_index);
			let base_genus_index: GIT = Library::get_base_genus_index(family_index).into();
			let family_genus_count: GIT = Library::get_family_genus_count(family_index);
			let genus_index: GIT = self.genus_index.into();

			self.genus_index = GenusIndex::try_from(
				(genus_index
					- base_genus_index
					+ if should_increment { 1 as GIT } else { family_genus_count - 1 as GIT }
				) % family_genus_count + base_genus_index
			).unwrap();
		}
	}
}

impl Default for InputToggles {
	fn default() -> Self {
		Self {
			enable_modifiers:		true,
			rotate_twice:			false,
			counter_clockwise:		false,
			alt_hemi:				false,
			disable_recentering:	false,
			genus_index:			GenusIndex::SIMPLE
		}
	}
}

#[derive(Default)]
pub struct InputState {
	pub puzzle_action:			Option<PuzzleAction>,
	pub file_action:			Option<FileAction>,
	pub camera_rotation:		Quat,
	pub has_camera_rotation:	bool,
	pub is_solving:				bool,
	pub toggles:				InputToggles,
}

impl InputState {
	pub fn has_active_action(&self) -> bool {
		self.puzzle_action.is_some() || self.file_action.is_some() || self.is_solving
	}

	fn reset_update_data(&mut self) -> () { self.has_camera_rotation = false; }
}

pub struct InputPlugin;

impl InputPlugin {
	fn run(
		extended_puzzle_state:		Res<ExtendedPuzzleState>,
		keyboard_input:				Res<Input<KeyCode>>,
		mouse_button_input:			Res<Input<MouseButton>>,
		preferences:				Res<Preferences>,
		view:						Res<View>,
		time:						Res<Time>,
		camera_query:				CameraQuery,
		mut mouse_motion_events:	EventReader<MouseMotion>,
		mut mouse_wheel_events:		EventReader<MouseWheel>,
		mut bevy_egui_context:		ResMut<BevyEguiContext>,
		mut input_state:			ResMut<InputState>
	) -> () {
		let input_state: &mut InputState = input_state.deref_mut();

		input_state.reset_update_data();

		if !matches!(*view, View::Main) {
			return;
		}

		let egui_context: &EguiContext = bevy_egui_context.ctx_mut();
		let input_data: &InputData = &preferences.input;
		let mut toggles: InputToggles = input_state.toggles;

		if !egui_context.wants_keyboard_input() {
			toggles.update(&input_data.key_presses, &*keyboard_input);
		}

		if !egui_context.wants_pointer_input() {
			let (camera_rotation, has_camera_rotation): (Quat, bool) = {
				const PAN_SCALING_FACTOR: f32 = 500_000.0_f32;
				const ROLL_SCALING_FACTOR: f32 = 5_000.0_f32;
	
				let (mouse_motion_delta, mouse_wheel_delta): (Vec2, f32) = {
					let time_delta: f32 = time.delta_seconds();
	
					(
						if mouse_button_input.pressed(MouseButton::Middle) {
							mouse_motion_events
								.iter()
								.map(|mouse_motion: &MouseMotion| -> &Vec2 {
									&mouse_motion.delta
								})
								.sum::<Vec2>() / time_delta
						} else {
							Vec2::ZERO
						},
						mouse_wheel_events
							.iter()
							.map(|mouse_wheel: &MouseWheel| -> f32 {
								mouse_wheel.y
							})
							.sum::<f32>() / time_delta
					)
				};
	
				if !mouse_motion_delta.abs_diff_eq(Vec2::ZERO, f32::EPSILON) { (
					Quat::from_axis_angle(
						Vec3::new(mouse_motion_delta.x, -mouse_motion_delta.y, 0.0_f32).cross(Vec3::Z).normalize(),
						mouse_motion_delta.length() / PAN_SCALING_FACTOR * preferences.speed.camera.pan_speed as f32
					),
					true
				) } else if mouse_wheel_delta.abs() > f32::EPSILON { (
					Quat::from_rotation_z(
						mouse_wheel_delta / ROLL_SCALING_FACTOR * preferences.speed.camera.roll_speed as f32
					),
					true
				) } else { (Quat::IDENTITY, false) }
			};

			input_state.camera_rotation = camera_rotation;
			input_state.has_camera_rotation = has_camera_rotation;
		}

		match (&mut input_state.puzzle_action, &mut input_state.file_action) {
			(None, None) => {
				Self::update_action(
					&extended_puzzle_state,
					&keyboard_input,
					&preferences,
					&camera_query,
					input_state
				);
			},
			(None, Some(file_action)) => {
				if let Some(file_handle) = file_action.poll() {
					if let Some(Some(file_name)) = file_handle
						.as_ref()
						.map(FileHandle::path)
						.map(Path::to_str)
					{
						match file_action.action_type {
							FileActionType::Save => {
								warn_expect_ok!(SaveState {
									extended_puzzle_state:	extended_puzzle_state.clone(),
									input_toggles:			toggles,
									camera:					CameraQueryNT(&camera_query)
										.orientation(|camera_orientation: Option<&Quat>| -> HalfAddr {
											camera_orientation
												.map_or(HalfAddr::default(), CameraPlugin::compute_camera_addr)
										})
								}.to_file(file_name));
							},
							FileActionType::Load => {
								warn_expect_ok!(SaveState::from_file(file_name), |save_state: SaveState| -> () {
									input_state.puzzle_action = Some(PuzzleAction {
										current_action: None,
										pending_actions: Some(Box::new(PendingActions::load(&save_state, &preferences.speed.animation))),
										action_type: PuzzleActionType::Load
									});
									input_state.toggles = save_state.input_toggles;
								});
							}
						}
					}
					
					input_state.file_action = None;
				}
			},
			(Some(active_action), None) => {
				if input_state.has_camera_rotation {
					if matches!(active_action.action_type, PuzzleActionType::RecenterCamera) {
						/* If the whole purpose of the current active transformation action is to recenter the camera,
						which we no longer wish to do, end the active transformation action entirely */
						input_state.puzzle_action = None;
					} else if let Some(current_action) = &mut active_action.current_action {
						// Cancel active camera movement
						current_action.has_camera_orientation = false;
					}
				}
			},
			(Some(_), Some(_)) => unreachable!()
		}

		input_state.toggles = toggles;
	}

	fn update_action(
		extended_puzzle_state:	&ExtendedPuzzleState,
		keyboard_input:			&Input<KeyCode>,
		preferences:			&Preferences,
		camera_query:			&CameraQuery,
		input_state:			&mut InputState
	) -> () {
		use KeyPressAction as KPA;

		let input_data:				&InputData					= &preferences.input;
		let speed_data:				&SpeedData					= &preferences.speed;
		let mut puzzle_action_type:	Option<PuzzleActionType>	= None;
		let mut default_position:	Option<usize>				= None;

		for rot_action_index in 0_usize .. HALF_PENTAGON_PIECE_COUNT {
			if input_data.key_presses[KPA::from_usize(rot_action_index)].is_active(keyboard_input) {
				puzzle_action_type = Some(PuzzleActionType::Transformation);
				default_position = Some(input_data.default_positions[rot_action_index]);

				break;
			}
		}

		if puzzle_action_type.is_none() {
			if input_data.key_presses[KPA::RecenterCamera].is_active(keyboard_input) {
				puzzle_action_type = Some(PuzzleActionType::RecenterCamera);
			} else if input_data.key_presses[KPA::Undo].is_active(keyboard_input)
				&& extended_puzzle_state.curr_action > 0_usize
			{
				puzzle_action_type = Some(PuzzleActionType::Undo);
			} else if input_data.key_presses[KPA::Redo].is_active(keyboard_input)
				&& extended_puzzle_state.curr_action < extended_puzzle_state.actions.len()
			{
				puzzle_action_type = Some(PuzzleActionType::Redo);
			}
		}

		if let Some(puzzle_action_type) = puzzle_action_type {
			let camera_orientation: Quat = CameraQueryNT(camera_query)
				.orientation(|camera_orientation: Option<&Quat>| -> Option<Quat> { camera_orientation.copied() })
				.unwrap_or_default();
			let camera_start: HalfAddr = CameraPlugin::compute_camera_addr(&camera_orientation);
			let recenter_camera: bool = matches!(puzzle_action_type, PuzzleActionType::RecenterCamera);
			let action: Action = match puzzle_action_type {
				PuzzleActionType::RecenterCamera => Action::new(
					FullAddr::default(),
					camera_start
				),
				PuzzleActionType::Transformation => Action::new(
					FullAddr::from((
						input_state.toggles.genus_index,
						input_state
							.toggles
							.half_addr(default_position.unwrap())
					)) + camera_start,
					camera_start
				),
				PuzzleActionType::Undo => extended_puzzle_state
					.actions
					[extended_puzzle_state.curr_action - 1_usize]
					.invert(),
				PuzzleActionType::Redo => extended_puzzle_state
					.actions
					[extended_puzzle_state.curr_action],
				_ => unreachable!()
			};

			if !info_expect!(matches!(puzzle_action_type, PuzzleActionType::RecenterCamera)
				|| action.is_valid()
				&& if action.transformation.is_genus_index_reorientation() {
					*action.transformation.get_half_addr() != action.camera_start
				} else {
					!action.transformation.is_identity_transformation()
				}
			) {
				debug_expr!(puzzle_action_type, action);

				return;
			}

			let (camera_orientation, has_camera_orientation): (Quat, bool) = if recenter_camera
				|| !input_state.toggles.disable_recentering
			{
				(camera_orientation, true)
			} else {
				(Quat::IDENTITY, false)
			};
			let start: Instant = Instant::now();
			let duration: Duration = action.compute_duration(&speed_data.animation, puzzle_action_type);

			input_state.puzzle_action = Some(
				PuzzleAction {
					current_action: Some(CurrentAction{
						camera_orientation,
						start,
						duration,
						action,
						has_camera_orientation
					}),
					pending_actions: None,
					action_type: puzzle_action_type
				}
			);
		}
	}
}

impl Plugin for InputPlugin {
	fn build(&self, app: &mut App) -> () {
		app
			.insert_resource(InputState::default())
			.add_system_to_stage(
				CoreStage::PreUpdate,
				Self::run
					.system()
					// .after(InputSystem)
					.after(EguiSystem::BeginFrame)
			);
	}
}