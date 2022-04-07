use {
	crate::{
		prelude::*,
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
		puzzle::{
			consts::*,
			transformation::{
				Action,
				Addr,
				FullAddr,
				GenusIndex,
				GenusIndexConsts,
				GenusIndexType,
				HalfAddr,
				Library,
				FullMask,
				RandHalfAddrParams
			},
			ExtendedPuzzleState,
			InflatedPuzzleState,
			InflatedPuzzleStateConsts
		},
	},
	super::{
		Preferences,
		View
	},
	std::{
		collections::VecDeque,
		mem::take,
		ops::DerefMut,
		path::Path,
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
		prelude::*,
		app::CoreStage,
		input::{
			keyboard::KeyCode as BevyKeyCode,
			mouse::{
				MouseMotion,
				MouseWheel
			}
		},
		utils::BoxedFuture
	},
	bevy_inspector_egui::{
		egui,
		Context,
		Inspectable
	},
	rand::{
		rngs::ThreadRng,
		Rng,
		thread_rng
	},
	rfd::FileHandle,
	serde::{
		Deserialize,
		Serialize
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
					.get_rotation_quat(rotation % PENTAGON_SIDE_COUNT as u32)
					.mul_vec3(face_1_norm), 
				None
			);
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

	fn ui(&mut self, ui: &mut egui::Ui, _: Self::Attributes, context: &mut Context) -> bool {
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
	pub struct InputData {
		#[inspectable(collapse)]
		pub default_positions:		[usize; HALF_PENTAGON_PIECE_COUNT]		= generate_default_positions(),
		#[inspectable(collapse)]
		pub rotation_keys:			[KeyCode; HALF_PENTAGON_PIECE_COUNT]	= [bkc!(Numpad0), bkc!(Numpad1), bkc!(Numpad4), bkc!(Numpad5), bkc!(Numpad6), bkc!(Numpad3)],
		pub recenter_camera:		KeyCode									= bkc!(Space),
		pub undo:					KeyCode									= bkc!(Left),
		pub redo:					KeyCode									= bkc!(Right),
		pub cycle_genus_index_up:	KeyCode									= bkc!(Up),
		pub cycle_genus_index_down:	KeyCode									= bkc!(Down),
		pub enable_modifiers:		KeyCode									= bkc!(E),
		pub rotate_twice:			KeyCode									= bkc!(D),
		pub counter_clockwise:		KeyCode									= bkc!(S),
		pub alt_hemi:				KeyCode									= bkc!(A),
		pub disable_recentering:	KeyCode									= bkc!(X)
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
							
							let s: f32 = s * FullAddr::get_cycles_for_comprising_simples(
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
							PuzzleActionType::Transformation | PuzzleActionType::Randomize => {
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
					+ PENTAGON_SIDE_COUNT as i32
			)
				as usize
				% PENTAGON_SIDE_COUNT
		} else {
			0_usize
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
		keyboard_input:				Res<Input<BevyKeyCode>>,
		mouse_button_input:			Res<Input<MouseButton>>,
		view:						Res<View>,
		time:						Res<Time>,
		preferences:				Res<Preferences>,
		camera_query:				CameraQuery,
		mut mouse_motion_events:	EventReader<MouseMotion>,
		mut mouse_wheel_events:		EventReader<MouseWheel>,
		mut input_state:			ResMut<InputState>
	) -> () {
		let input_state: &mut InputState = input_state.deref_mut();

		input_state.reset_update_data();

		if !matches!(*view, View::Main) {
			return;
		}

		let input_data: &InputData = &preferences.input;
		let mut toggles: InputToggles = input_state.toggles;

		macro_rules! check_toggle {
			($toggle:ident) => {
				if keyboard_input.just_pressed(input_data.$toggle.into()) { toggles.$toggle = !toggles.$toggle; }
			}
		}

		check_toggle!(enable_modifiers);
		check_toggle!(rotate_twice);
		check_toggle!(counter_clockwise);
		check_toggle!(alt_hemi);
		check_toggle!(disable_recentering);

		if keyboard_input.just_pressed(input_data.cycle_genus_index_up.into()) {
			toggles.genus_index = GenusIndex::try_from(
				if usize::from(toggles.genus_index) == 0_usize {
					Library::get_genus_count() as GenusIndexType
				} else {
					*toggles.genus_index
				} - 1 as GenusIndexType
			).unwrap();
		}

		if keyboard_input.just_pressed(input_data.cycle_genus_index_down.into()) {
			toggles.genus_index = GenusIndex::try_from(
				if usize::from(toggles.genus_index) == Library::get_genus_count() - 1_usize {
					0 as GenusIndexType
				} else {
					*toggles.genus_index + 1 as GenusIndexType
				}
			).unwrap();
		}

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
								log_result_err!(SaveState {
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
		keyboard_input:			&Input<BevyKeyCode>,
		preferences:			&Preferences,
		camera_query:			&CameraQuery,
		input_state:			&mut InputState
	) -> () {
		let input_data:				&InputData					= &preferences.input;
		let speed_data:				&SpeedData					= &preferences.speed;
		let mut puzzle_action_type:	Option<PuzzleActionType>	= None;
		let mut default_position:	Option<usize>				= None;

		for (index, key_code) in input_data.rotation_keys.iter().enumerate() {
			if keyboard_input.just_pressed((*key_code).into()) {
				puzzle_action_type = Some(PuzzleActionType::Transformation);
				default_position = Some(input_data.default_positions[index]);

				break;
			}
		}

		if puzzle_action_type.is_none() {
			if keyboard_input.just_pressed(input_data.recenter_camera.into()) {
				puzzle_action_type = Some(PuzzleActionType::RecenterCamera);
			} else if keyboard_input.just_pressed(input_data.undo.into())
				&& extended_puzzle_state.curr_action > 0_usize
			{
				puzzle_action_type = Some(PuzzleActionType::Undo);
			} else if keyboard_input.just_pressed(input_data.redo.into())
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
			.add_system_to_stage(CoreStage::PreUpdate, Self::run.system());
	}
}