#' @details
#'
#' **To visualize sample data**, such as a data distribution, samples from a
#' bootstrap distribution, or a Bayesian posterior, you can supply samples to
#' the `x` or `y` aesthetic.
#'
#' **To visualize analytical distributions**, you can use the `xdist` or `ydist`
#' aesthetic. For historical reasons, you can also use `dist` to specify the distribution, though
#' this is not recommended as it does not work as well with orientation detection.
#' These aesthetics can be used as follows:
#'
#'  - `xdist`, `ydist`, and `dist` can be any distribution object from the [distributional](https://pkg.mitchelloharawild.com/distributional/)
#'    package ([dist_normal()], [dist_beta()], etc) or can be a [posterior::rvar()] object.
#'    Since these functions are vectorized,
#'    other columns can be passed directly to them in an [aes()] specification; e.g.
#'    `aes(dist = dist_normal(mu, sigma))` will work if `mu` and `sigma` are columns in the
#'    input data frame.
#'
#'  - `dist` can be a character vector giving the distribution name. Then the  `arg1`, ... `arg9`
#'    aesthetics (or `args` as a list column) specify distribution arguments. Distribution names
#'    should correspond to R functions that have `"p"`, `"q"`, and `"d"` functions; e.g. `"norm"`
#'    is a valid distribution name because R defines the [pnorm()], [qnorm()], and [dnorm()]
#'    functions for Normal distributions.
#'
#'    See the [parse_dist()] function for a useful way to generate `dist` and `args`
#'    values from human-readable distribution specs (like `"normal(0,1)"`). Such specs are also
#'    produced by other packages (like the `brms::get_prior` function in brms); thus,
#'    [parse_dist()] combined with the stats described here can help you visualize the output
#'    of those functions.
#'
