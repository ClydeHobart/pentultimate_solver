use bevy_inspector_egui::{
	egui::{
		self,
		CollapsingHeader,
		Color32,
		Ui
	},
	Context,
	Inspectable
};

pub trait KVP {
	type Key: Clone + Default + Inspectable + Ord;
	type Value: Default + Inspectable;

	fn key(&self) -> &Self::Key;
	fn value(&self) -> &Self::Value;
	
	fn key_mut(&mut self) -> &mut Self::Key;
	fn value_mut(&mut self) -> &mut Self::Value;
}

impl<K, V> KVP for (K, V)
	where
		K: Clone + Default + Inspectable + Ord,
		V: Default + Inspectable
{
	type Key = K;
	type Value = V;

	fn key(&self) -> &Self::Key { &self.0 }
	fn value(&self) -> &Self::Value { &self.1 }

	fn key_mut(&mut self) -> &mut Self::Key { &mut self.0 }
	fn value_mut(&mut self) -> &mut Self::Value { &mut self.1 }
}

pub struct InspectableBinMapMut<'a, T: Default + KVP> {
	map: &'a mut Vec<T>
}

impl<'a, T: Default + KVP> InspectableBinMapMut<'a, T> {
	fn label_button(ui: &mut Ui, text: &str, text_color: Color32) -> bool {
		ui.add(egui::Button::new(text).text_color(text_color).frame(false)).clicked()
	}

	pub fn get(&mut self, k: &<T as KVP>::Key) -> Option<&mut <T as KVP>::Value> {
		match self.map.binary_search_by_key(k, |t: &T| -> <T as KVP>::Key { t.key().clone() }) {
			Ok(index) => { Some(self.map[index].value_mut()) },
			Err(_) => { None }
		}
	}
}

impl<'a, T: Default + KVP> Inspectable for InspectableBinMapMut<'a, T> {
	type Attributes = (<<T as KVP>::Key as Inspectable>::Attributes, <<T as KVP>::Value as Inspectable>::Attributes);

	fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, context: &Context) -> bool {
		let mut changed: bool = false;
		let mut key_changed: bool = false;

		ui.vertical(|ui: &mut Ui| {
			let len: usize = self.map.len();
			let mut to_delete: Option<usize> = None;

			for (i, kvp) in self.map.iter_mut().enumerate() {
				ui.horizontal(|ui: &mut Ui| -> () {
					if Self::label_button(ui, "✖", Color32::RED) {
						to_delete = Some(i);
					}

					key_changed |= kvp.key_mut().ui(
						ui,
						options.0.clone(),
						&context.with_id((2usize * i) as u64)
					);
					ui.label(":");
					CollapsingHeader::new(format!("value {}", i))
						.id_source((2_usize * i + 1_usize) as u64)
						.show(ui, |ui| {
							changed |= kvp.value_mut().ui(
								ui,
								options.1.clone(),
								&context.with_id((2_usize * i + 1_usize) as u64)
							);
						});
				});

				if i != len - 1 {
					ui.separator();
				}
			}

			ui.vertical_centered_justified(|ui: &mut Ui| {
				if ui.button("+").clicked() {
					self.map.push(T::default());
					changed = true;
				}
			});

			if let Some(i) = to_delete {
				self.map.remove(i);
				changed = true;
			}

			if key_changed {
				self.map.sort_by_cached_key(|t: &T| -> <T as KVP>::Key { t.key().clone() });
				self.map.dedup_by_key(|t: &mut T| -> <T as KVP>::Key { t.key().clone() });
			}
		});

		changed || key_changed
	}
}

pub trait AsInspectableBinMapMut<T: Default + KVP> {
	fn as_inspectable_bin_map_mut<'a>(&'a mut self) -> InspectableBinMapMut<'a, T>;
}

impl<T: Default + KVP> AsInspectableBinMapMut<T> for Vec<T> {
	fn as_inspectable_bin_map_mut<'a>(&'a mut self) -> InspectableBinMapMut<'a, T> {
		InspectableBinMapMut::<T> {
			map: self
		}
	}
}

pub struct InspectableBinMap<'a, T: Default + KVP> {
	map: &'a Vec<T>
}

impl<'a, T: Default + KVP> InspectableBinMap<'a, T> {
	pub fn get(&self, k: &<T as KVP>::Key) -> Option<&<T as KVP>::Value> {
		match self.map.binary_search_by_key(k, |t: &T| -> <T as KVP>::Key { t.key().clone() }) {
			Ok(index) => { Some(self.map[index].value()) },
			Err(_) => { None }
		}
	}
}

pub trait AsInspectableBinMap<T: Default + KVP> {
	fn as_inspectable_bin_map<'a>(&'a self) -> InspectableBinMap<'a, T>;
}

impl<T: Default + KVP> AsInspectableBinMap<T> for Vec<T> {
	fn as_inspectable_bin_map<'a>(&'a self) -> InspectableBinMap<'a, T> {
		InspectableBinMap::<T> {
			map: self
		}
	}
}