--- 
title: "Syllabus: Meaningful Text Analysis with Word Embeddings" 
authors: 
 - Jonathan Reeve
---

A week-long workshop, taught at [Digital Humanities Summer Institute, Summer 2021](https://dhsi.org/dhsi-2021-online-edition).

**Instructor**: [Jonathan Reeve](https://jonreeve.com), Department of English and Comparative Literature, Columbia University. 

**Email**: [jonathan.reeve@columbia.edu](mailto:jonathan.reeve@columbia.edu). **Although please communicate with me using the course chatroom, whenever possible and appropriate.**

**Website**: <https://dhsi2021.jonreeve.com>

**Dates**: 14–18 June, 2021, at [11:00 Victoria / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), for one hour.

**Classroom**: <https://meet.jit.si/dhsi2021-word-embeddings>

**Chatroom**: [DHSI2021 Word Embeddings, on Matrix](https://matrix.to/#/!NfrKxmoXbgbyylMTzE:matrix.org?via=matrix.org). We'll use this room for all of our communication. A good program for Matrix chat is [Element](https://element.io/), available for Web, Android, iOS, MacOS, Windows, and Linux.

**WARNING**: This syllabus is very much still a work-in-progress, and likely won't be complete until the course start date, in June 2021.

## Course Description

Word embeddings provide new ways of understanding language, by encorporating contexts, meanings, and senses of words into their digital representations. They are a new technology, developed by researchers at Google, which now powers the most advanced computational language tasks, such as machine translation, automatic summarization, and information extraction. Since they represent more than just the surface forms of words, their applications for humanities scholarship are profound. This course will serve as a hands-on introduction to word embeddings, and will use the Python programming language, in conjunction with the SpaCy package for natural language processing. Participants are encouraged to bring their own collections of text to analyze, and will create meaningful explorations of them by the end of the course. No prior programming experience is necessary.

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

 - Please [fill out this short initial survey](https://docs.google.com/forms/d/e/1FAIpQLSd52frST_WDm5rdXJ6zobMDIL0IjyBqt8QXmGMU90hK1tRxPw/viewform?usp=sf_link), whether you are a participant, auditor, or anyone else.
 - Please [introduce yourself to everyone in our course chatroom](https://matrix.to/#/!NfrKxmoXbgbyylMTzE:matrix.org?via=matrix.org). You may have to create a Matrix account.
 - [Create a Hypothes.is account](https://hypothes.is/signup), if you don't already have one, and [write an annotation on our first reading](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/jurafsky.pdf). I recommend using your real name as your username, so that it's easier to know who's who.

## Monday, 14 June: Theory of Word Embeddings 

 - Lecture video: TBA. A link will appear here as soon as it's available.
 - Class videoconference:  [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2021-word-embeddings).
 - Reading: [Chapter 6 of Jurafski, Dan, and James H. Martin. *Speech and Language Processing*. Third edition draft.](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/jurafsky.pdf)
   - Please write at least one annotation using the Hypothes.is annotation layer.
   - [Original here](https://web.stanford.edu/~jurafsky/slp3/6.pdf)

## Tuesday, 15 June: Introduction to Python for Text Analysis

 - Lecture video: TBA. A link will appear here as soon as it's available.
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2021-word-embeddings).
 - [Mikolov, Tomas and Chen, Kai and Corrado, Greg and Dean, Jeffrey. "Efficient estimation of word representations in vector space." arXiv preprint arXiv:1301.3781.](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/mikolov.pdf)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Wednesday, 16 June: Hands-on With Pre-Trained Word Embeddings

 - Lecture video: TBA. A link will appear here as soon as it's available.
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2021-word-embeddings).
 - Reading: [Kozlowski, Austin C., Matt Taddy, and Evans, James A. (2019) "The Geometry of Culture: Analyzing the Meanings of Class through Word Embeddings." American Sociological Review 84:5.](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/kozlowski.pdf)
   - [Also available here, via Sage](https://journals.sagepub.com/doi/full/10.1177/0003122419877135)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Thursday, 17 June: Practicum in Text Analysis

 - Lecture video: TBA. A link will appear here as soon as it's available.
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2021-word-embeddings).
 - Reading: [Garg, N., Schiebinger, L., Jurafsky, D., and Zou, J. (2018) "Word embeddings quantify 100 years of gender and ethnic stereotypes" PNAS 115:16](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/garg.pdf)
   - [Originally here, at PNAS](https://www.pnas.org/content/115/16/E3635.short)
   - Please write at least one annotation using the Hypothes.is annotation layer.

## Friday, 18 June: Lab Work

 - Lecture video: TBA. A link will appear here as soon as it's available.
 - Class videoconference: [11:00 Pacific / 14:00 New York / 18:00 UTC](https://time.is/compare/1400_14_June_2021_in_New_York/UTC/Victoria,_British_Columbia), in [our videoconference room on Jitsi](https://meet.jit.si/dhsi2021-word-embeddings).
 - Reading: [Bolukbasi, Tolga, Kai-Wei Chang, James Zou, Venkatesh Saligrama, and Adam Kalai. "Man is to computer programmer as woman is to homemaker? debiasing word embeddings." arXiv preprint arXiv:1607.06520 (2016).](https://via.hypothes.is/https://dhsi2021.jonreeve.com/static/readings/bolukbasi.pdf)
   - [Original via ArXiv](https://arxiv.org/abs/1607.06520)
   - Please write at least one annotation using the Hypothes.is annotation layer.
