.PHONY: document readme vignettes test check format lint lint-lintr lint-all style site coverage

document:
	Rscript -e "devtools::document()"

readme:
	Rscript -e "devtools::build_readme()"

vignettes:
	Rscript -e "devtools::build_vignettes()"

test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"

format:
	Rscript -e "air::format_package()"

lint:
	Rscript -e "jarl::lint_package()"

lint-lintr:
	Rscript -e "lintr::lint_package()"

lint-all: lint lint-lintr

style: format

site:
	Rscript -e "pkgdown::build_site()"

coverage:
	Rscript -e "covr::package_coverage()"
