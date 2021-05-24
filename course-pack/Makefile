course-pack.pdf: course-pack.tex syllabus.tex jurafsky-ch-6.pdf mikolov2013efficient.pdf Kozlowski_2019.pdf Garg_2018.pdf
	lualatex $<
	rm *.aux *.log *.out *.toc

syllabus.tex: ../content/00-syllabus.md
	pandoc --wrap=none $< -o $@
