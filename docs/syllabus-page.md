## Syllabus

Classical and simulation-based techniques for estimation and hypothesis testing,
including inference for means and proportions. Emphasis on case studies and
real data sets, as well as reproducible and transparent workflows when writing
computer scripts for analysis and reports.

### Prerequisites

- DSCI 100: Introduction to Data Science.
- Access to a computer.
  - If a student does not have their own laptop or chromebook, students may be able to [loan a laptop from the UBC library](https://services.library.ubc.ca/computers-technology/technology-borrowing/).

### When and where?

- Classes are on Mondays and Wednesdays from 4:00 PM to 5:30 PM in CHEM B250

### Teaching Team
- Instructor: Dr. Lasanatha Premarathna (wpremara@stat.ubc.ca)

 #### Teaching Assistants
 - Marc Wettengel
 - Yifan Yin
 - Daniel Zhezlov 
 - Abhinav Kansal 

### Office Hours
Times are in Pacific Time.

- Tue: Marc (9am - 10am) - Zoom link on Canvas
- Wednesday: Lasantha (11am-12pm) - Zoom link on Canvas
- Thursday: Yifan (10am - 11am) - Zoom link on Canvas
- Friday: Daniel (10-11am) - Zoom link on Canvas
- Friday: Abhinav (5:30-6:30pm) - Zoom link on Canvas

**Note**:
<!--  Monday Sep 5th is UBC Imagine Day, so we will have both lecture + tutorial on Wednesday Sep 7th 
      Monday Oct 10th is Thanksgiving, so we will have both lecture + tutorial on Wednesday Oct 12th 
      Nov 9-11th are mid-term breaks, so we will have both lecture + tutorial on Monday Nov 7th           -->
- Feb 20-24th is the  mid-term break

#### Test Dates

- Midterm 1: Wednesday, Feb 8, 2023 (in class) 
- Midterm 2: Wednesday, March 15, 2023 (in class)
- Final Exam: To be scheduled by Classroom Services

### Software Platforms

- Students will learn to perform their analysis using the [R programming language](https://cran.r-project.org/).
- Worksheets and tutorial problem sets as well as the final project analysis, development, and reports
will be done using [Jupyter Notebooks](http://jupyter.org/).
- Students will access the worksheets and tutorials in Jupyter Notebooks through Canvas. 

### Textbooks

- ModernDive: Statistical Inference via Data Science developed by Chester Ismay and Albert Y. Kim.
  - This book is freely available: [https://moderndive.com/]https://moderndive.com/)
- OpenIntro Statistics by David Diez, Mine Cetinkaya-Rundel, and Christopher Barr which is available by donation.
  - This book is available by donation (whatever you can afford): [https://www.openintro.org/book/os/](https://www.openintro.org/book/os/)

### Learning Outcomes

By the end of the course, students will be able to: 

- Describe real-world examples of questions that can be answered with the statistical inference
methods presented in this course (e.g., estimation, hypothesis testing) and apply inference skills and
concepts to answer such questions.
- Explain what random and representative samples are and how they can influence estimation.
- Write computer scripts to perform estimation and hypothesis testing via simulation-based inference
approaches, as well as by applying results from exact and approximate distributional theory.
- Interpret and explain results from confidence intervals and hypothesis tests.
- Compare the application of simulation-based inference approaches with the application of exact and
approximate distributional results.
- Effectively visualize point estimates and different measures of uncertainty (e.g., confidence
intervals, standard errors) by writing computer scripts.
- Discuss the impact of type I & II errors as well as responsible use and reporting of p-values on
hypothesis tests.
- Explain estimator bias and uncertainty, and write a computer script to calculate it.
- Discuss how an estimator's bias arises (e.g., sample bias, study design), and its implications in
statistical inference.
- Perform all aspects of a statistical analysis (from data consumption to reporting) using
reproducible and transparent computer scripts.

### Assessments
#### Winter Term 1 and 2 semesters:
Each week there will be two assignments: (1) worksheets; and (2) tutorials.
The assignments will be due every Saturday, 23:59:59. The worksheets are
fully autograded with visible tests to help you identify points that need
more clarification. Therefore, reach out to the teaching team if you don't
understand why you are getting an answer wrong in the worksheet. On the
other hand, the tutorials are not fully autograded, and only a few exercises
will have visible tests. You can access the assignments through Canvas assignments.

#### Summer semester only:
_Usually_ each week there will be four assignments: (1) two worksheets; and (2) two tutorials.
However, some weeks will vary. Please see Canvas for the due dates for each week.
Assignments will be due every Saturday, 23:59:59. The worksheets are
fully autograded with visible tests to help you identify points that need
more clarification. Therefore, reach out to the teaching team if you don't
understand why you are getting an answer wrong in the worksheet. On the
other hand, the tutorials are not fully autograded, and only a few exercises
will have visible tests. You can access the assignments through Canvas assignments.

To submit your assignment, make sure your work is saved
**on our server** (i.e., using the link from Canvas) before the deadline.
Our server will automatically snapshot at the due date/time. Also, please **do not** rename the
assignments files.

#### Assessments' Weights

<div style="display: flex; height: max-content;">
    <table>
        <caption>Table 1: Course</caption>
        <thead>
            <tr>
                <th>Deliverable</th>
                <th>Weight</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Worksheets</td>
                <td>5%</td>
            </tr>
            <tr>
                <td>Tutorials</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Group project</td>
                <td>20%</td>
            </tr>
            <tr>
                <td>Midterm 1</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Midterm 2</td>
                <td>15%</td>
            </tr>
            <tr>
                <td>Final Exam</td>
                <td>30%</td>
            </tr>
        </tbody>
    </table>
</div>

  <div style="display: flex; height: max-content;">
    <table>
        <caption>Table 2: Project</caption>
        <thead>
            <tr>
                <th>Deliverable</th>
                <th>Weight</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Team Work Contract</td>
                <td>1%</td>
            </tr>
            <tr>
                <td>Proposal</td>
                <td>4%</td>
            </tr>
            <tr>
                <td>Peer review</td>
                <td>3%</td>
            </tr>
            <tr>
                <td>Final Report</td>
                <td>9%</td>
            </tr>
            <tr>
                <td>Teammate Evaluation</td>
                <td>3%</td>
            </tr>
 </tbody>
    </table>
    </div>
    
#### Exams
All the exams will be open-book. You can use whatever (*but not whoever*) you want. You **are not allowed** to communicate with another person during the exams. For all the exams, the platform used will be Canvas. The types of questions can vary: reasoning, multiple-choice, multiple-answer,
dropdown, true or false. Although most questions will be about the content, you can expect a few coding
questions. That being said, the coding question will not be overly complicated, and we will only check your
familiarity with the main functions and packages we use in the course.
We **are not** trying to test your memory!!! Please don't spend energy trying to memorize
everything. If you had done the worksheets and tutorial, this should not be a problem for you.

- **Midterms:** two 45-minute mid-terms will be administered at the start
of the tutorial in Weeks 5 and 9 (or Weeks 3 and 5 in Summer Terms). The first midterm will cover
Weeks 1 to 4 (i.e., Worksheets/Tutorials 1, 2, 3, and 4, plus
the readings). The second midterm will <em>focus</em> on Weeks 6, 7, and 8 (i.e.,
Worksheets/Tutorials 1, 2, 3, and 4, plus
the readings), however, the content is cumulative as the concepts are dependent.
- **Final Exam:** The final exam will be a 90-minute exam and it will cover the material of
the entire course. Notwithstanding the grading weights <a href="#course-breakdown">above</a>,
students must pass the final exam to pass the course.

### Policies

####  Late/Absence

- Regular attendance to lecture and tutorials is expected of students.
Students who are unavoidably absent from numerous classes because of illness or other reasons
should inform the instructor(s) of the course as soon as possible, preferably,
prior to the start of the lecture/tutorial. 
- There will be no make-up exams. Students who miss Midterm 1, Midterm 2
or an assignment and want to request an Academic Concession need to contact
the Instructor as soon as possible and provide a <a href="">self-declaration form</a>.
Failing to present a declaration may result in a grade of zero on the assessment.
- Late submissions of **worksheets** and **tutorials**
will receive a grade of 0. Note that one worksheet and one tutorial will be dropped at the end of the semester. 
If you have extenuating circumstances and need concessions beyond dropping one worksheet and one tutorial, please email the instructor. 
- For other assessments, late submission is defined as
any work submitted after the deadline. Late submissions
will receive a late penalty.

####  Autograder Policy 

Many of the questions in assignments are graded automatically by software.
The grading computer has exactly the same hardware setup as the server that
students work on. No assignment, when completed, should take longer than 5
minutes to run on the server. The autograder will automatically stop (time out)
for each student assignment after a maximum of 5 minutes;
*any ungraded questions at that point will receive a score of 0.*


Furthermore, students are responsible for making sure their assignments are *reproducible*,
and run from beginning to end on the autograding computer. In particular, *please ensure that any data that needs to be downloaded is done so by the assignment notebook with the correct filename to the correct folder.* A common mistake is to manually download data when
working on the assignment, making the autograder unable to find the data and often resulting in
an assignment grade of 0. Even small mistakes such as using the wrong sample size will justify
an incorrect response for that (and possibly downstream) questions.

**In short: whatever grade the autograder returns after 5 minutes (assuming the teaching team did not make an error) is the grade that will be assigned.**

Tip: when you're done the assignment, click "Restart and Run All",
and check that the autograder returns the results you are expecting.

#### Regrading
If you have concerns about the way your work was graded, please open a request
within one week of having the grade returned to you. After this one-week window,
we may deny your request for re-evaluation. Also, please keep in mind that your
grade may go up or down as a result of re-grading. To open a regrade requests,
please follow the steps below:

1. Go to Piazza and click on <code>New post</code>.
2. In <code>Post Type</code>, select <code>Question</code>.
3. Make the post private to instructors and TAs only. In <code>Post to</code> select <code>Individual Students(s)/Instructor(s)</code>. A text box will appear, where you must type <code>Instructors</code>.
4. In <code>Select Folder(s)</code> select the folder <code>regrading</code>
5. In <code>Summary</code> say the Assignment you want to be regraded,
followed by the question and your name and student number.
For example, <code>lab 3 -> Q3 -- Rodolfo Lourenzutti (9982313)</code>
6.  Provide a brief reason for why the regrade is needed.
7. The TAs will see the request and will take a look at the assignment. If necessary, they will involve the instructors.
Finally, once the TA is finished reassessing the assignment:
  - If the grade deserves more marks: the TA will update the mark on Canvas and comment on the question so everyone can see that the question has been addressed.
  - If your grade goes down or stays the same: the TA will answer the post on Piazza, giving the student a reason for their final decision

####  Device/Browser

Students are responsible for using a device and browser compatible with all functionality of Canvas.
Chrome or Firefox browsers are recommended; Safari has had issues with Canvas quizzes in the past.

####  Missed Final Exam 

Students who miss the final exam must report to their faculty advising office within
72 hours of the missed exam, and must supply supporting documentation.
Only your faculty advising office can grant deferred standing in a course.
You must also notify your instructor prior to (if possible) or immediately after the exam.
Your instructor will let you know when you are expected to write your deferred exam.
Deferred exams will ONLY be provided to students who have applied for and received
deferred standing from their faculty.

#### Academic Concession Policy

Please see [UBC's concession policy](http://www.calendar.ubc.ca/vancouver/index.cfm?tree=3,329,0,0) for detailed information on dealing with missed coursework, quizzes, and exams under circumstances of an acute and unanticipated nature.

####  Academic Integrity 

The academic enterprise is founded on honesty, civility, and integrity.
As members of this enterprise, all students are expected to know, understand, and
follow the codes of conduct regarding academic integrity. At the most basic level,
this means submitting only original work done by you and acknowledging all sources
of information or ideas and attributing them to others as required. This also means
you should not cheat, copy, or mislead others about what is your work. Violations of
academic integrity (i.e., misconduct) lead to the breakdown of the academic
enterprise, and therefore serious consequences arise and harsh sanctions are
imposed. For example, incidences of plagiarism or cheating may result in a mark of zero on the
assignment or exam and more serious consequences may apply if the matter is referred to the
President's Advisory Committee on Student Discipline. Careful records are kept in order to
monitor and prevent recurrences.

A more detailed description of academic integrity, including the University's policies and procedures,
may be found in the [Academic Calendar](http://calendar.ubc.ca/vancouver/index.cfm?tree=3,54,111,0).

####  Plagiarism 

Students must correctly cite any code or text that has been authored
by someone else or by the student themselves for other assignments.
Cases of plagiarism may include, but are not limited to:

1. the reproduction (copying and pasting) of code or text with none or minimal reformatting (e.g., changing the name of the variables)
2. the translation of an algorithm or a script from a language to another
3. the generation of code by automatic code-generation software

An “adequate acknowledgement” requires a detailed identification of the (parts of the) code or text reused and a full citation of the original
source code that has been reused.

The above attribution policy applies only to assignments. **No code or text may be copied (with or without attribution) from any source during a quiz or exam.** Answers must always be in your own words. At a minimum, copying will result in a grade of 0 for the related question.

Repeated plagiarism of any form could result in larger penalties, including failure of the course.

### Attribution 

This syllabus was copied almost in its entirety from the [DSCI 100 syllabus](https://github.com/UBC-DSCI/dsci-100/blob/master/README.md), with only minor modifications. Parts of DSCI 100 syllabus, hence of this syllabus as well, have been copied from the [UBC MDS Policies](https://ubc-mds.github.io/policies/).
