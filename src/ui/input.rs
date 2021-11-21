use {
	crate::{
		prelude::*,
		math::polyhedra::{
			data::{
				Data,
				FaceData
			},
			Polyhedron
		},
		puzzle::consts::*
	},
	super::Preferences,
	bevy::{
		prelude::*,
		app::CoreStage,
		input::keyboard::KeyCode as BevyKeyCode
	},
	bevy_inspector_egui::{
		egui,
		Context,
		Inspectable
	},
	serde::Deserialize
};

pub fn generate_default_positions() -> [usize; HALF_PENTAGON_PIECE_COUNT] {
	let mut positions: [usize; HALF_PENTAGON_PIECE_COUNT] = [0_usize; HALF_PENTAGON_PIECE_COUNT];
	let icosidodecahedron_data: &Data = match Data::get(Polyhedron::Icosidodecahedron) {
		Some(icosidodecahedron_data) => icosidodecahedron_data,
		None => {
			return positions;
		}
	};
	let face_0_data: &FaceData = &icosidodecahedron_data.faces[0_usize];
	let face_1_norm: Vec3 = icosidodecahedron_data.faces[1_usize].norm;

	for rotation in 3_u32 ..= 7_u32 {
		positions[rotation as usize - 2] = icosidodecahedron_data.get_closest_face_index(&face_0_data.get_rotation_quat(rotation % PENTAGON_SIDE_COUNT as u32).mul_vec3(face_1_norm), None);
	}

	positions
}

#[derive(Clone, Copy, Deserialize, PartialEq)]
#[serde(from = "bevy::input::keyboard::KeyCode")]
pub struct KeyCode(BevyKeyCode);

impl From<BevyKeyCode> for KeyCode { fn from(bevy_key_code: BevyKeyCode) -> Self { Self(bevy_key_code) } }
impl From<KeyCode> for BevyKeyCode { fn from(key_code: KeyCode) -> Self { key_code.0 } }

impl Inspectable for KeyCode {
	type Attributes = ();

	fn ui(&mut self, ui: &mut egui::Ui, _: Self::Attributes, context: &Context) -> bool {
		let mut changed: bool = false;

		egui::ComboBox::from_id_source(context.id())
			.selected_text(format!("{:?}", self.0))
			.show_ui(ui, |ui: &mut egui::Ui| -> () {
				macro_rules! ui_for_key_codes {
					($($variant:ident),*) => {
						$(
							if ui.selectable_label(
									matches!(self.0, BevyKeyCode::$variant),
									format!("{:?}", BevyKeyCode::$variant)
							).clicked() {
								self.0 = BevyKeyCode::$variant;
								changed = true;
							}
						)*
					}
				}

				ui_for_key_codes!(
					Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, Key0,
					A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
					Escape,
					F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
					F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24,
					Snapshot,
					Scroll,
					Pause,
					Insert,
					Home,
					Delete,
					End,
					PageDown, PageUp,
					Left, Up, Right, Down,
					Back,
					Return,
					Space,
					Compose,
					Caret,
					Numlock, Numpad0, Numpad1, Numpad2, Numpad3, Numpad4, Numpad5, Numpad6, Numpad7, Numpad8, Numpad9,
					NumpadAdd,
					NumpadSubtract,
					NumpadMultiply,
					NumpadDivide,
					NumpadDecimal,
					NumpadComma,
					NumpadEnter,
					NumpadEquals,
					AbntC1, AbntC2,
					Apostrophe,
					Apps,
					Asterisk,
					Plus,
					At,
					Ax,
					Backslash,
					Calculator,
					Capital,
					Colon,
					Comma,
					Convert,
					Equals,
					Grave,
					Kana,
					Kanji,
					LAlt,
					LBracket,
					LControl,
					LShift,
					LWin,
					Mail,
					MediaSelect,
					MediaStop,
					Minus,
					Mute,
					MyComputer,
					NavigateForward,
					NavigateBackward,
					NextTrack,
					NoConvert,
					Oem102,
					Period,
					PlayPause,
					Power,
					PrevTrack,
					RAlt,
					RBracket,
					RControl,
					RShift,
					RWin,
					Semicolon,
					Slash,
					Sleep,
					Stop,
					Sysrq,
					Tab,
					Underline,
					Unlabeled,
					VolumeDown, VolumeUp,
					Wake,
					WebBack, WebFavorites, WebForward, WebHome, WebRefresh, WebSearch, WebStop,
					Yen,
					Copy, Paste, Cut
				);
			});

		changed
	}
}

macro_rules! bkc {
	($variant:ident) => {
		KeyCode::from(BevyKeyCode::$variant)
	}
}


define_struct_with_default!(
	#[derive(Clone, Deserialize, Inspectable, PartialEq)]
	pub InputData {
		#[inspectable(collapse)]
		pub default_positions:		[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		#[inspectable(collapse)]
		pub rotation_keys:			[KeyCode; HALF_PENTAGON_PIECE_COUNT]	= [bkc!(Numpad0), bkc!(Numpad1), bkc!(Numpad4), bkc!(Numpad5), bkc!(Numpad6), bkc!(Numpad3)],
		pub rotate_twice:			KeyCode									= bkc!(D),
		pub counter_clockwise:		KeyCode									= bkc!(S),
		pub alt_hemi:				KeyCode									= bkc!(A),
		pub recenter_camera:		KeyCode									= bkc!(Space),
		pub disable_recentering:	KeyCode									= bkc!(X),
	}
);

pub enum PendingAction {
	None,
	Transformation{ default_position: usize },
	RecenterCamera
}

impl Default for PendingAction { fn default() -> Self {	Self::None } }

#[derive(Default)]
pub struct InputState {
	pub rotate_twice:			bool,
	pub counter_clockwise:		bool,
	pub alt_hemi:				bool,
	pub disable_recentering:	bool,
	pub pending_action:			PendingAction
}

pub struct InputPlugin;

impl InputPlugin {
	fn run(
		keyboard_input: Res<Input<BevyKeyCode>>,
		preferences: Res<Preferences>,
		mut input_state: ResMut<InputState>
	) -> () {
		let input_data: &InputData = &preferences.input;

		macro_rules! check_toggle {
			($key_code:ident, $toggle:ident) => {
				if keyboard_input.just_pressed(input_data.$key_code.into()) { input_state.$toggle = !input_state.$toggle; }
			}
		}

		check_toggle!(rotate_twice,			rotate_twice);
		check_toggle!(counter_clockwise,	counter_clockwise);
		check_toggle!(alt_hemi,				alt_hemi);
		check_toggle!(disable_recentering,	disable_recentering);

		let mut pending_action: PendingAction = PendingAction::None;

		for (default_position, key_code) in input_data.rotation_keys.iter().enumerate() {
			if keyboard_input.just_pressed((*key_code).into()) {
				pending_action = PendingAction::Transformation{ default_position: input_data.default_positions[default_position] };

				break;
			}
		}

		if matches!(pending_action, PendingAction::None) && keyboard_input.just_pressed(input_data.recenter_camera.into()) {
			pending_action = PendingAction::RecenterCamera;
		}

		input_state.pending_action = pending_action;
	}
}

impl Plugin for InputPlugin {
	fn build(&self, app: &mut AppBuilder) -> () {
		app
			.insert_resource(InputState::default())
			.add_system_to_stage(CoreStage::PreUpdate, Self::run.system());
	}
}