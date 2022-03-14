{application, animal_world,
 [{description, "An animal world simulation"},
  {vsn, "1.0.0"},
  {mod, {animal_world, []}},
  {registered, []},
  {modules, [animal, animal_sup, world, world_sup, db]},
  {applications, [stdlib, kernel, mnesia]}]}.
