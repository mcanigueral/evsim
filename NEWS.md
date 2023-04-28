# evsim 1.0.0

# evsim 0.1.0

* Bug fix in the `evmodel` class printing function
* Adding california EV model as example evmodel.
* Function "simulate_sessions" now requires a "user_profiles" parameter with the ratio of every user profile and the optional specific power.
* Add function "get_user_profiles_distribution" to facilitate the creation of the newly requested parameter "user_profiles" by function "simulate_sessions"
* Remove function `update_profiles_ratios` in favour of `prepare_models`. This new function is used inside the `simulate_sessions` function using the `sessions_day` and `user_profiles` parameters to modify the `evmodel` input.
