# Contributing

We are sincerely grateful for your interest in contributing to STAT 200 website. 

Here are instructions to create a content page (a page that will hold content like chapters, exercises, etc.):
1. Download the `new_page.html` in the `Templates` folder.
2. Write your content in the main section of the page (you can search for the main in the file).
    - There are examples of all current elements on the template page. Please use those to improve the consistency across the pages. 
    We will use that format to keep track of all definitions, examples, and exercises on the website. Besides, by using those elements, any design updates will immediately affect your page as well. 

If you need a new element, there is no problem with creating one. 
Just make sure to add the new element to the template page, so the writer of a new page will know the existence of the new element. We are more than happy to help you create a new element. You can open an issue requesting the element (maybe describing a little how you envision it). 

You can update your page's title (that shows on the tab of the browser) by editing this element:
```html
<title>STAT 200: My topic </title>
```

Once you have written  wrote your page, you need to decide where it will go on the website. After moving the file there, make sure to fix the path in the following elements:
```html
<link rel="stylesheet" href="../styles/sub-pages-main.css">
<link rel="stylesheet" href="../styles/chapter.css">
<script defer src="../scripts/exercises-controller.js"></script>
<script defer src="../scripts/sampling-dist.js"></script>
<img src="../imgs/UBC-logo-blue.png" />
```

