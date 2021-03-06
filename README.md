# Meaningful Text Analysis with Word Embeddings

A website for the course, Meaningful Text Analysis with Word Embeddings, taught at the [Digital Humanities Summer Institute](https://dhsi.org/dhsi-2021-online-edition/) in Summer 2021. 

This repository just contains the source code for the website. [See here for the main course website](http://dhsi2021.jonreeve.com/)

WARNING: this syllabus is still a work-in-progress, and likely won't be complete until closer to the course start date, in June of 2021. If you have any questions about the course, don't hesitate to reach out: jonathan.reeve@columbia.edu.

## Students: submit your final projects

Students, to submit your final projects, and have them published here, submit a pull request to this repository, where you add your Jupyter notebook file to the `content` folder. See [the example](https://github.com/JonathanReeve/course-word-embeddings/blob/master/content/first-post.md). 

If you've never written markdown before, see [this cheat sheet](https://www.markdownguide.org/cheat-sheet). 

And if you've never submitted a pull request before, see [the instructions in the example](https://github.com/JonathanReeve/course-word-embeddings/blob/master/content/first-post.md), or [one of the many tutorials for GitHub pull requests](https://duckduckgo.com/?q=github+pull+request&ia=web). Let us know if you have any questions.

## Building

Students typically won't need to do this. But if you want to build and run this site locally, for some reason, make sure you have Nix installed, and then run:

```bash
nix-shell --run 'ghcid -T ":main -wS"'
```

This launches a web server at http://localhost:8080 serving the statically generated content. Changing either `./src/Main.hs` or the content in `./content` reloads everything.

