suppressMessages({
  pkgload::load_all("../cohortBuilder", quiet = TRUE)
  pkgload::load_all(".", quiet = TRUE)
})
library(magrittr)
iris2 <- iris
iris2$Species <- as.character(iris2$Species)
coh <- cohort(set_source(tblist(iris = iris2)), compute_stats = TRUE, propagate_domains = "filter") %>%
  add_filter(filter("discrete", id = "species", dataset = "iris", variable = "Species",
                    domain = c("setosa", "versicolor", "virginica")))

# Simulate render attaching GUI to step 1
coh$modify(function(public, private) {
  private$steps[["1"]] <- shinyCohortBuilder:::attach_filters_gui(private$steps[["1"]])
})
cat("step1 gui attached:", shinyCohortBuilder:::step_gui_attached(coh$get_step("1")), "\n")

# Now copy_step -> creates step 2 by cloning step 1 (which HAS gui)
coh$copy_step(run_flow = FALSE)
s2 <- coh$get_step("2")
cat("step2 exists:", !is.null(s2),
    " gui attached on clone:", shinyCohortBuilder:::step_gui_attached(s2), "\n")
if (!is.null(s2)) {
  g <- s2$filters[["species"]]@private$gui
  cat("step2 gui class:", paste(class(g), collapse = ","),
      " update is.function:", is.function(g$update), "\n")
  cat("gui names:", paste(names(g), collapse = ","), "\n")
}
