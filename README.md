# University of Edinburgh - School of Psychology<br>Statistics Courses

---
Structure of the document  
============================  
<!--ts-->
1. [Welcome](#welcome)  
1. [Purpose of this github organisation](#purpose-of-this-github-organisation)  
1. [Repositories](#repositories)
1. [Course repository structures](#course-repository-structures)
1. [How do I contribute?](#how-do-i-contribute)
1. [Introduction to version control systems](#introduction-to-version-control-systems)  
1. [Writing](#writing)  
    - [Templates](#templates)
    - [Course colours](#course-colour-schemes)
    - [Lectures](#lectures)
    - [Labs](#labs)
    - [Reports](#reports)
1. [Publishing](#publishing)  
    - [Lectures](#publishing-lectures)
    - [Labs](#publishing-labs)
1. [Accessing](#accessing)  
1. [Linking from Learn or external](#linking-eg-from-learn)  
1. [Editing the website](#editing-the-website)
<!--te-->

---

## <a name="welcome"></a>Welcome

Welcome to the GitHub organisation for the Statistics courses of the School of Psychology of the University of Edinburgh!

The purpose of this organisation is to have a unique place where up-to-date documents are stored (lecture or labs), and new content is drafted.
The centralised structure will help people finding the content that you developed, even if you will not be teaching the course in one particular year.
Most importantly, it also provides a front-end for publishing materials, available at [uoepsy.github.io](https://uoepsy.github.io).  

By linking Learn to materials on these pages, students will always see **up-to-date content**. Did students flag a typo in your lecture slides? Don't worry! You just need to update the file here and there is no need to re-upload everything on Learn all the time.

The next sections will introduce you to the structure of the organisation, introduce you to version control systems, explain where to write your material and how to publish it online. Next, it will explain how to access the material you have prepared and how to link it to Blackboad Learn to make it visible to your students.

Thanks for reading!


## Purpose of this github organisation

The goal of this GitHub organisation is to store up-to-date content of lectures and labs so that you can just share a link to the course content on Learn, and students are taken to the most recent version (perhaps you fixed a typo).  

GitHub "organisations" are essentially github accounts which have no named holder, but instead are maintained and worked on by a number of named contributors.  

The organisation has a number of repositories, detailed in the next section.  

## Repositories

+ __website repository:__ uoepsy.github.io  
The main repository for editing the out-ward facing styles is named _uoepsy.github.io_  and contains html for the main page of the website, along with .css for all pages.   

+ __course repositories:__  
Each course has its own github repository. This is a place for us to work on materials, _as well as_ holding the published versions.
    - *dapr1* (course content for the UG course Data analysis for Psychology in R 1)
    - *dapr2* (course content for the UG course Data analysis for Psychology in R 2)
    - *dapr3* (course content for the UG course Data analysis for Psychology in R 3)
    - *usmr* (course content for the PG course Univariate statistics and methodology in R)
    - *msmr* (course content for the PG course Multivariate statistics and methodology in   


+ __templates repository:__ templates  
The templates repository contains all the most up-to-date templates for labs, lectures and courseworks. These are then used across the course repositories to produce the materials.  

## Course repository structures  

The same basic repository structure is followed a cross course repositories, for instance, for the __dapr1__ repository, there are the following subfolders:   
+ __dapr1_labs/__ _work on the labs here_
+ __dapr1_lectures/__ _work on the lectures here_
+ __docs/__ _everything inside this folder gets published_
    + __docs/labs/__ _published labs_
    + __docs/lectures/__ _published lectures_

Everything inside the **docs** folders will get rendered appropriately and can be linked to via url.  
for a given course, the **docs/** folder corresponds to uoepsy.github.io/[course name]

It is good practice to develop course material in the course-specific folders on the master branch.


## How do I contribute?

1. Obtain a GitHub account
1. Install the [GitKraken Git GUI client](https://www.gitkraken.com/download)
1. Clone the online repository from GitHub to your PC
1. Create or edit course material on your local copy of the repository
1. Make sure to transfer your work to the repository stored online on GitHub


**Step 1. Obtain a GitHub account**.
If you are involved with teaching or tutoring quantitative courses in this school, the first thing you should do is to register for a GitHub account:

- Go to [GitHub](https://github.com/)
- Click "Sign up" on the top right corner
- Choose a username, and write your academic email and password
- Once you have created an account it is good practice to [verify yourself as an educator](https://education.github.com/)

You will use your GitHub account in order to contribute to the statistics repository.

**Step 2. Install the [GitKraken Git GUI client](https://www.gitkraken.com/download)**.
You will use this software in order to communicate from your PC to the remote folder stored on the GitHub website.

- [How to install GitKraken](https://support.gitkraken.com/how-to-install/)
- [Connect GitKraken to your GitHub account](https://support.gitkraken.com/start-here/profiles/)

**Step 3. Clone the online repository from GitHub to your PC**.
You now want to clone (i.e. copy) the online uoe_psystats repository to your local PC.
In order to do that:

- Open GitKraken
- click File
- select Clone Repo
- select Github.com
- _Where to clone to_: browse to the location on your PC where you want the online folder to be copied to
- _URL_: write the url of the GitHub repository https://github.com/uoepsy/dapr1.git

Check out the help page for [cloning an existing project](https://support.gitkraken.com/working-with-repositories/open-clone-init/).


**Step 4. Create or edit course material on your local copy of the repository**.
Now you can create or edit course content on your local copy of the online folder. Remember, each course has a specific subfolder where the material should be.

We recommend each course to have the subfolders **lectures, labs**.

Once you have finished creating a lecture:

- go back to GitKraken
- on the right hand side click stage file or stage all changes, so that any further changes to those files will be tracked
- once you are happy with your changes, write a short description in the "Commit Message" pane, and you can commit your work, i.e. create a local snapshot of your file.

**Step 5. Make sure to transfer your work to the repository stored online on GitHub**.
One you have the snapshot of your files, you want to update the online repository to match your local one.

First, you want to click the Pull button from the GitKraken bar, which has a downwards arrow, in order for you to get on your local folder the most recent changes done to the online folder.

Typically, if you are working in your course-specific folder, you will not have issues of people overwriting your files, and you will not have problems with the merging.

Then, click the Push button from the GitKraken bar, which has an upwards arrow icon. This will take your changes and upload them to the online repository stored on GitHub.



## Introduction to version control systems

How do you keep track of changes made to a specific file?
We all did that - starting from FILE.txt saved on Dropbox, and then changing something and saving another file called FILE_final.txt.
Perhaps you want to switch two paragraphs? FILE_really_final.txt.
Then someone gives you feedback and you have to change the order of two sections: FILE_really_really_final.txt

At the end we have in the Dropbox folder a series of files

1. FILE.txt
1. FILE_final.txt
1. FILE_really_final.txt
1. FILE_really_really_final.txt

whose names, I have to say, are not really informative, and it's hard to remember what changed from file 2 to file 3.

Version control systems are tools that let you keep track of the changes made to a file, without having to create many different copies of the file with different names.
Git is an example of version control systems, and it is a command-line program. The online website GitHub is an online storage of Git repositories, making it easier to collaborate and share your work with other people.

Instead of using command-line tools, we will use the GitKraken graphical user interface to Git.

So, in short, you will only have to use

- GitHub: to store your online copy of the repository and share it with others
- GitKraken: to interact with GitHub from your computer

There are important terms to be familiar with when using GitKraken.

- Stage: you stage a file when you want to keep track of changes
- Commit: you create a snapshot of the file (i.e., snapshot of the changes made)
- Push: push your snapshot online to GitHub
- Pull: pull the changes done by others to the online GitHub repository onto your local folder, so that you are working on the up-to-date version of the folder.

It is good practice to do Pull before you start working on a file, and also before you Push to GitHub.

# Writing   

### Templates

In the `templates/` repository, you will find different examples for writing lectures, labs, and reports.  All of which are written in Rmarkdown.   

- Lectures are compiled as `.html`  
- Labs are compiled into a bookdown (a `.html` book with each lab as a chapter). The option also exists to build to `.pdf` should printouts be required.  
- Reports are compiled to `.pdf` and then uploaded to LEARN

**Important:** Do not use github repositories to store actual reports (or drafts). Everything here is public!  

### Course colour schemes  

Our courses use the following colour schemes:

- DAPR1: ![](https://placehold.it/15/0F4C81/000000?text=+) `#0F4C81`
- DAPR2: ![](https://placehold.it/15/BF1932/000000?text=+) `#BF1932`
- DAPR3: ![](https://placehold.it/15/88B04B/000000?text=+) `#88B04B`
- USMR: ![](https://placehold.it/15/FCBB06/000000?text=+) `#FCBB06`
- MSMR: ![](https://placehold.it/15/a41ae4/000000?text=+) `#a41ae4`

Where possible, all materials (lectures, labs, reports etc.) corresponded to given course will follow the designated colour scheme.  
How to set the colour scheme for each course is detailed in the sections below explaining how to write different types of content.  

### Lectures

For lectures, the template we provide uses [`xaringan` package](https://github.com/yihui/xaringan).  

You can find a tutorial on how to produce slides using `xaringan` [here](https://slides.yihui.org/xaringan/#1).  

```{r}
install.packages('xaringan')
devtools::install_github("gadenbuie/xaringanthemer")
```

#### Setting the course colour  

At the top of each lecture `.Rmd`, there is a code chunk which sets the colour for the slides:     

```{r xaringan-themer, include = FALSE}
library(xaringanthemer)
mono_accent(
  base_color = "#0F4C81", # DAPR1
  # base_color = "#e41a1c", # DAPR2
  # base_color = "#4daf4a", # DAPR3
  # base_color = "#FCBB06", # USMR
  # base_color = "#a41ae4", # MSMR
  header_color = "#000000",
  header_font_google = google_font("Roboto Condensed"),
  text_font_google   = google_font("Roboto", "300", "300i"),
  code_font_google   = google_font("Source Code Pro")
)
```

Simply un-comment the appropriate course `base_color` setting (DAPR1 in the example above) to change the colour scheme for the compiled lecture.  


### Labs

Our labs are written and compiled as bookdowns, but also built as `.pdfs` should students require printouts.

Each lab begins with the same structure:

- Learning objectives
- Readings (where applicable)

After that point, courses vary in lab structure - some contain walkthroughs, some are just questions.  

#### Drop-down questions/solution boxes  

Our labsheets have the ability to include dropdown question and solution boxes.  

To write a question:    
```
`r qbegin()`  
This is the question. It will have a heading of "Question"
`r qend()`  
```

You can add question numbers/labels, using:
```
`r qbegin(1)`  
This Q will end up with a heading of "Question 1"
`r qend()`  

`r qbegin("example 1",qlabel=FALSE)`  
This Q will end up with a heading of "example 1"
`r qend()`  
```

To include solutions:  
```
`r solbegin(show=TRUE)`  
This is the solution. When rendered, it will be visible, and it will have a heading of "Solution"  
`r solend(show=TRUE)`  

`r solbegin(show=FALSE)`  
This is the solution. When rendered, it will NOT be visible
`r solend(show=FALSE)`  

`r solbegin(show=TRUE, label="peppapig 1", slabel=FALSE)`  
This is the solution. When rendered, it will be visible, and it will have a heading of "peppapig 1".
`r solend(show=TRUE)`  
```

You can render with and without solutions shown by setting a variable such as HIDE_SOLUTIONS at the top of each lab which is either TRUE/FALSE, and then passing that variable to the `solbegin()` and `solend()` functions (e.g., `r solbegin(show=HIDE_SOLUTIONS)`).

#### Boxes  

There are a number of coloured divs we use to identify various things throughout the labs.  

+ Question boxes (see above) are coloured red.
+ Solution boxes (expandable, see above) are coloured green.
+ Learning Objectives = lightblue
+ Definitions/Important = yellow
+ Caution = red
+ Tips = green
+ Extra content = frame (a black frame, clear background)

To use these in the bookdown, simply:
```
:::yellow
this content will be inside a yellow box. with lovely rounded corners :)
:::
```

#### Setting the course colour  

Open the `index.Rmd` for the bookdown.  
In the yaml front matter at the top of this file, the title contains reference to svg colour squares.  

This adds to the title a coloured square corresponding to the course colour scheme. The number of squares corresponds to the year to which the course is offered.  

For instance, for dapr1, one square is included:  
```
---
  title:  <svg width="50" height="50">
      <rect x="0" y="0" width="50" height="50"
      style="fill:#0F4C81;stroke:none;opacity:1" />
    </svg>
    <br>
    Data Analysis for Psychology in R 1
author: "Department of Psychology, The University of Edinburgh"
```

Edit the svg style to the correct HEX colour code and repeat the appropriate number of times.


### Reports  

Reports are written in Rmarkdown and compiled to pdf.  
The title includes the same coloured squares as for the labsheets, set in the yaml front matter.
Change the filepath to the correct image for the course you are writing for.

```
---
title: |
  ![](images/dapr1.png){width=0.51in} \protect \linebreak
  Data Analysis for Psychology in R 1  \protect \linebreak
  Coursework report
```

**Important: DO NOT work on actual reports in this repository. everything is public**  

## Publishing  

### Publishing lectures  

1. __Copy content__  
Any finished **and rendered**  lectures should be placed in the `docs/lectures/` folder (along with any image folders).    
1. __Update list__
Edit the `docs/lectures/index.md` file and add the link to the lecture you just added. If, for instance, you just added a lecture entitled `lecture8_probability.html`, add a line such as:  
```
[Lecture 8 - Probability](lecture8_probability.html)
```

The `docs/lectures/index.md` file corresponds to [uoepsy.github.io/[course name]/lectures/](uoepsy.github.io/dapr1/lectures/)

### Publishing labs

To compile the bookdown to `.html`, open the `index.Rmd` and click knit. This will call `bookdown::render_book()`.  

To build both `.html` and `.pdf`: in Rstudio menu: Build > Build All.  

__Important:__ These will automatically build into `../docs/labs/`. If you compile the book but don't want to update the materials, then __do not stage the docs/ files__.

To publish, simply stage all files!  

## Accessing  

A github page is built from the `docs/` folder of each repository, meaning that any `.html` within `docs/` are rendered appropriately and can be linked to from elsewhere.  

In a browser, navigating to [https://uoepsy.github.io](https://uoepsy.github.io) will take you to the the published homepage (this is the uoepsy.github.io repository).  

To access your published material, simply append the appropriate filepath to this url.   
For example, if you are working on the __dapr2__ repository, and have put a lecture titled `dapr2-lecture3.html` inside `docs/lectures/`, then https://uoepsy.github.io/dapr2/lectures/dapr2-lecture3.html will take you to the published file.

## Linking (e.g. from Learn)  

To link to the pages, simply navigate to the appropriate path at  [https://uoepsy.github.io](https://uoepsy.github.io), and create a link in Learn.  

Links between lectures and labs follow the same procedure. For instance, to include a link in a lab to a lecture:

```
The lecture slides are available [here](https://uoepsy.github.io/dapr2/lectures/dapr2-lecture3.html)
```

## Editing the website  

There are three levels to the website:  
+ __main page__ (https://uoepsy.github.io/) - the __uoepsy.github.io__ repository contains `index.html` file for the main page, along with .css styles and icon images.  
The `index.html` contains links to each course page, e.g., to https://uoepsy.github.io/dapr1/
+ __course pages__ (e.g., https://uoepsy.github.io/dapr1/) - each course repository contains `docs/index.html` which contains a course description, and links to `labs/` and `lectures/`. Styles and images are sourced from the main page repo.  
+ __lecture/lab pages__
    + lab pages (e.g, https://uoepsy.github.io/dapr1/labs) are the bookdown compiled `index.htmls` - will update each build of book.
    +  lecture pages (e.g., https://uoepsy.github.io/dapr1/lectures) are the `docs/lectures/index.md` files in each course repository. They require manual updating to link to each lecture file. 
