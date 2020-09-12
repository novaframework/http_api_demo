#{prefix => "",
  security => false,
  routes => [
            {"/pet", { http_api_demo_pet_controller, manage_pet}, #{methods => [options, get, post]}},
            {"/pet/:petid", { http_api_demo_pet_controller, manage_pet}, #{methods => [options, get, put]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
