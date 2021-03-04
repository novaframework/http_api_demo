#{prefix => "",
  security => false,
  routes => [
            {"/pet", { http_api_demo_pet_controller, get_pets}, #{methods => [options, get]}},
            {"/pet", { http_api_demo_pet_controller, create_pet}, #{methods => [options, post]}},
            {"/pet/:petid", { http_api_demo_pet_controller, get_pet}, #{methods => [options, get]}},
            {"/pet/:petid", { http_api_demo_pet_controller, update_pet}, #{methods => [options, put]}},
            {"/pet/:petid", { http_api_demo_pet_controller, remove_pet}, #{methods => [options, delete]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
