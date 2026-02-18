# ---- User Commands ----

student-html:
	Rscript build.R student gitbook

student-pdf:
	Rscript build.R student pdf

tutor-html:
	Rscript build.R tutor gitbook

tutor-pdf:
	Rscript build.R tutor pdf

# Build everything
all: student-html student-pdf tutor-html tutor-pdf

# Clean bookdown cache
clean:
	rm -rf _bookdown_files _bookdown_cache


# make student-html
# make student-pdf
# make tutor-html
# make tutor-pdf
# make all