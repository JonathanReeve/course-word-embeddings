--- 
title: "Meaningful Text Analysis with Word Embeddings" 
authors: 
 - Jonathan Reeve
---

# Syllabus

A week-long workshop, taught at [Digital Humanities Summer Institute, Summer 2022](https://dhsi.org/dhsi-2022-online-edition).

**Instructor**: [Jonathan Reeve](https://jonreeve.com), Department of English and Comparative Literature, Columbia University. 

**Email**: [jonathan.reeve@columbia.edu](mailto:jonathan.reeve@columbia.edu). **Although please communicate with me using the course chatroom, whenever possible and appropriate.**

**Website**: <https://dhsi2022.jonreeve.com>

**Dates**: 6–10 June, 2022, at [11:00 Victoria / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_06_June_2022_in_New_York/UTC/Victoria,_British_Columbia), for one hour.

**Classroom**: <https://meet.jit.si/dhsi2022-word-embeddings>

**Chatroom**: [DHSI2022 Word Embeddings, on Matrix](https://matrix.to/#/%23dhsi2022-word-embeddings:matrix.org). We'll use this room for all of our communication. A good program for Matrix chat is [Element](https://element.io/), available for Web, Android, iOS, MacOS, Windows, and Linux.


## Course Description

Word embeddings provide new ways of understanding language, by incorporating contexts, meanings, and senses of words into their digital representations. They are a new technology, developed by researchers at Google, which now powers the most advanced computational language tasks, such as machine translation, automatic summarization, and information extraction. Since they represent more than just the surface forms of words, their applications for humanities scholarship are profound. This course will serve as a hands-on introduction to word embeddings, and will use the Python programming language, in conjunction with the SpaCy package for natural language processing. Participants are encouraged to bring their own collections of text to analyze, and will create meaningful explorations of them by the end of the course. No prior programming experience is necessary.

## Course Communications

Since this is an online-only course, this summer, we'll have to get creative with the ways we communicate. Here are our modes of communication: 

1. Daily video lectures. These are pre-recorded video lectures, to be watched before we meet each day. 
2. Daily videoconferences. These are our class times. 
3. Chatroom. This is for any other communication. Any questions or comments you may have, feel free to post them there. 
4. Marginalia, using Hypothes.is, on our readings. 

## Readings

I've chosen five readings that I hope will be of interest to you. I made the unconventional decision, for a digital humanities course, of choosing primary texts from technical disciplines, and so they may seem somewhat like they're written in a foreign language. Don't worry about understanding every bit of them. But don't ignore their implied challenge, either. 

We'll discuss the readings using Hypothes.is. Feel free to write any annotations you may have, in the virtual margins, and to reply to other annotations. Try to write at least one per reading.

## Technical stack 

We'll be using [Google Colaboratory](https://colab.research.google.com/) as our computing environment. It runs in the cloud, on Google's servers, so you don't need anything more than a web browser to run it. It does require that you have a Google account, however.

One important note about Colab is that the virtual machine's state (its memory of executed code) is wiped after a certain period of inactivity, around one hour. 

## Before the course 

 - Please [fill out this short initial survey](https://docs.google.com/forms/d/e/1FAIpQLScdm6Jlsv8iwzYquPCVpC4DlX9fRv98G3KPSvEvszYBuQhWkg/viewform?usp=sf_link), whether you are a participant, auditor, or anyone else.
 - Please [introduce yourself to everyone in our course chatroom](https://matrix.to/#/%23dhsi2022-word-embeddings:matrix.org). You may have to create a Matrix account.
 - [Create a Hypothes.is account](https://hypothes.is/signup), if you don't already have one, and [write an annotation on our first reading](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/jurafsky.pdf). I recommend using your real name as your username, so that it's easier to know who's who.

## Monday, 6 June: Theory of Word Embeddings 

 - [Lecture video 1](https://tubedu.org/videos/watch/312b6e65-0cfa-4a66-aed8-ff2176b4138c)
 - [NB: the lecture video is from 2021, bu applies to this year's course, as well. Please watch the lecture video before we meet over videoconference.]
 - [Colab notebook as a GitHub Gist](https://colab.research.google.com/gist/JonathanReeve/efd664f9b8af89f8a5f64c99e699a753/01-dhsi-word-embeddings.ipynb)
 - Class videoconference:  [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2022_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2022-word-embeddings).
 - Reading: [Chapter 6 of Jurafski, Dan, and James H. Martin. *Speech and Language Processing*. Third edition draft.](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/jurafsky.pdf)
   - Please write at least one annotation using the Hypothes.is annotation layer, before class.
   - [Original here](https://web.stanford.edu/~jurafsky/slp3/6.pdf)

## Tuesday, 7 June: Introduction to Python for Text Analysis

 - [Lecture video 2](https://tubedu.org/videos/watch/5746e37d-7581-4386-831b-e406cb6bb946). Please watch before class. 
 - [Lecture notebook 2](https://gist.github.com/JonathanReeve/250faf906992ee9973f5f4e907bbd8a1)
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2022_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2022-word-embeddings).
 - [Mikolov, Tomas and Chen, Kai and Corrado, Greg and Dean, Jeffrey. "Efficient estimation of word representations in vector space." arXiv preprint arXiv:1301.3781.](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/mikolov.pdf)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Wednesday, 8 June: Hands-on With Pre-Trained Word Embeddings

 - [Lecture video 3](https://tubedu.org/videos/watch/ac781240-2c94-477b-a413-0c15b0bba193)
 - [Lecture notebook 3](https://gist.github.com/JonathanReeve/d80571afa44ea45c83e67220d8544af4)
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2022_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2022-word-embeddings).
 - Reading: [Kozlowski, Austin C., Matt Taddy, and Evans, James A. (2019) "The Geometry of Culture: Analyzing the Meanings of Class through Word Embeddings." American Sociological Review 84:5.](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/kozlowski.pdf)
   - [Also available here, via Sage](https://journals.sagepub.com/doi/full/10.1177/0003122419877135)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Thursday, 9 June: Practicum in Text Analysis

 - [Lecture video 4](https://tubedu.org/videos/watch/ac76ef73-4a3c-46a4-8620-ef806a7c2104)
 - [Lecture notebook 4](https://gist.github.com/JonathanReeve/912b9c294306abd5d1351afea1f7da5e)
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2022_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2022-word-embeddings).
 - Reading: [Garg, N., Schiebinger, L., Jurafsky, D., and Zou, J. (2018) "Word embeddings quantify 100 years of gender and ethnic stereotypes" PNAS 115:16](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/garg.pdf)
   - [Originally here, at PNAS](https://www.pnas.org/content/115/16/E3635.short)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Friday, 10 June: Lab Work

 - [Lecture video 5](https://tubedu.org/videos/watch/b2aeded5-5a51-4d4e-be7a-c3e024e6db99)
 - [Lecture notebook 5](https://gist.github.com/JonathanReeve/7b9b3f9c23bb34f551ad48a84c4760ff)
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2022_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2022-word-embeddings).
 - Reading: [Bolukbasi, Tolga, Kai-Wei Chang, James Zou, Venkatesh Saligrama, and Adam Kalai. "Man is to computer programmer as woman is to homemaker? debiasing word embeddings." arXiv preprint arXiv:1607.06520 (2016).](https://via.hypothes.is/https://dhsi2022.jonreeve.com/static/readings/bolukbasi.pdf)
   - [Original via ArXiv](https://arxiv.org/abs/1607.06520)
   - Please write at least one annotation using the Hypothes.is annotation layer.
