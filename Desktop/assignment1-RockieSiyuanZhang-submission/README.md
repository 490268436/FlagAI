# COMP90054 AI Planning for Autonomy - Assignment 1 - Search 

You must read fully and carefully the assignment specification and instructions detailed in this file. You are NOT to modify this file in any way.

* **Course:** [COMP90054 AI Planning for Autonomy](https://handbook.unimelb.edu.au/subjects/comp90054) @ Semester 1, 2023
* **Instructor:** Dr. Nir Lipovetzky and Prof. Tim Miller
* **Deadline:** Friday 24th March, 2023 @ 11:59pm (end of Week 4)
* **Course Weight:** 10%
* **Assignment type:**: Individual (you can work with another student on the code, but submit your own repo and self-evaluation)
* **ILOs covered:** 1, 2, and 3
* **Submission method:** via GitHub using tagging (see [Submission Instructions](#submission-instructions) below for instructions)

The **aim of this assignment** is to get you acquainted with AI search techniques and how to derive heuristics in Pacman, as well as to understand how to model a problem with python.

 <p align="center"> 
    <img src="logo-p1.jpg" alt="logo project 1">
 </p>

## Ungrading
In COMP90054, we will use ungrading for the coursework component of the subject. Even though we are required to provide a grade out of 100, throughout the assignments, subject staff will not assign grades to anyone. Instead, we will use a combination of techniques that uses each student’s reflection of their own learning to provide grades. 

#### Why use ungrading? 

Who is better at assessing how much a student has leant: the student themselves, or a subject tutor/coordinator? I don’t imagine anyone would disagree that the student does. So why don’t we give students a say? For the coursework component of COMP90054 (assignments 1-3), the model that we employ cedes the power responsibility for monitoring and assessing progress to the students themselves. 

Research shows that grades and rubrics have three reliable effects on students in a class: 
1. they tend to think less deeply; 
2. they avoid taking risks; and 
3. they lose interest in the learning itself, We want to encourage all three of these things. 

How will a student know if they are doing well? They will receive feedback on assessment throughout the semester. Feedback will be qualitative with broad ratings: needs further work; good; or outstanding; but these ratings will NOT be used for grading. Our detail feedback will be focused on your self-evaluation to facilitate and maximise your learning. 

#### Contract grading


#### Collaboration
Assignments 1 can be completed individually or in pairs. We encourage pairs to work together to learn from each other; not to simple split the tasks for efficiency. But we will not monitor this – it is your responsibility. You can submit different solutions, we just want to encourage collaboration.

<!-- Each submission will contain an individual short self-reflection. -->
For the student works in pair, you must submit an individual short self evaluation ([SELFEV.md](SELFEV.md)). In addition, you can either submit the same coding solution (both need to submit in their own repo using tag), or submit different coding solution. 

We encourage students to derive their own tests and share them with others to help with learning. 

## Your tasks


Since the purpose of assignments is to help you learn more, marks should not be assigned only on whether your code passes a few test cases but also on how much you have learnt.

Who knows better about whether you have learnt from this assignment other than yourself? We ask you to evaluate your work.

Each submission will contain a short self-reflection on what the student learnt, how they approached the tasks (including writing new tests) and will give the student a chance to argue that, even though they didn’t complete a task, they tried and learnt from it. 

<!-- We will provide the feedback of your code, which from running the autograder on the server.  -->
Your task contains programming excercises with increasing difficulty. This is where we give students control over how much assessment they want to complete or have the time to complete.
* [Programming Tasks](#programming-tasks):
    *  [Practice](#practice)
    *  [Part 0 (0 marks)](#part-0-0-mark-but-critical)
    *  [Part 1 (3 marks)](#part-1-3-marks)
    *  [Part 2 (3 marks)](#part-2-3-marks)
    *  [Part 3 (4 marks)](#part-3-4-marks)
* [Self Evaluation Task](#self-evaluation-task)
* [Submission Instruction](#submission-instructions)


<!-- If you want to provide a report with your submission (e.g., reflections, acknowledgments, etc.), please do so in file [REPORT.md](REPORT.md). -->
### Programming Tasks:

You **must build and submit your solution** using the sample code we provide you in this repository, which is different from the original [UC Berkley code base](https://inst.eecs.berkeley.edu/~cs188/fa18/project1.html). 

* Please remember to complete the [SELFEV.md](SELFEV.md) file with your individual submission details (so we can identify you when it comes time to submit). 

* You should **only work and modify** files [search.py](search.py) and [searchAgents.py](searchAgents.py) in doing your solution. Do not change the other Python files in this distribution.

* Your code **must run _error-free_ on Python 3.8**. Staff will not debug/fix any code. Using a different version will risk your program not running with the Pacman infrastructure or autograder. 

* Your code **must not have any personal information**, like your student number or your name. That info should go in the [SELFEV.md](SELFEV.md) file, as per instructions above. If you use an IDE that inserts your name, student number, or username, you should disable that.

* **Assignment 1 FAQ** is available to answer common questions you might have about [Assignment 1 on ED](https://edstem.org/au/courses/10995/discussion/1183362)

* **Getting started on GitHub** - the video below explains how to **clone**, **git add**, **commit** and **push** while developing your solution for this assignment:

[![How to work with github](img/loom_video.png)](https://www.loom.com/share/ae7e93ab8bec40be96b638c49081e3d9)

#### Setting up the environment

* You can set up your local environment:
    * You can install Python 3.8 from the [official site](https://peps.python.org/pep-0569/), or set up a [Conda environment](https://www.freecodecamp.org/news/why-you-need-python-environments-and-how-to-manage-them-with-conda-85f155f4353c/) or an environment with [PIP+virtualenv](https://uoa-eresearch.github.io/eresearch-cookbook/recipe/2014/11/26/python-virtual-env/). 
    * You need to install additional package (func_timeout) using:  `pip3 install func_timeout`

* Alternatively, you can use docker:
    * You need to install docker from the [official site](https://docs.docker.com/get-docker/)
    * Please check [Docker Run](#docker-run) to run your code.




#### Practice

To familiarize yourself with basic search algorithms and the Pacman environment, it is a good start to implement the tasks at https://inst.eecs.berkeley.edu/~cs188/fa18/project1.html, especially the first four tasks; however, there is no requirement to do so.

You should code your implementations *only* at the locations in the template code indicated by ```***YOUR CODE HERE***``` in files [search.py](search.py) and [searchAgents.py](searchAgents.py), please do not change code at any other locations or in any other files.


#### Part 0 (0 mark, but critical)


This is a great way to test that you understand the submission instructions correctly, and how to get feedback from our hidden test-cases as many times as you want. Here are the steps:

* Please tag your solution with `test-submission`. If you are not familiar with tag, please check [tag hints](#git-hints-on-tags)
* We are going to run your code in our server. You can check your result from this [link](http://comp90054.ml/) after a few minutes. This can test your code for part 1, 2 and 3.

#### Part 1 (3 marks)

Implement the **Enforced Hill Climbing (EHC) algorithm** discussed in lectures, using Manhattan Distance as the heuristic, by inserting your code into the template indicated by comment ```***YOUR CODE HERE FOR TASK 1***```. You can see the code location following this link: [search.py#L150](search.py#L150).

> **Note** 
> You don't have to implement Manhattan Distance, as this has already been implemented for you in the codebase. You will need to call the heuristic from your implementation of EHC. You should be able to test the algorithm using the following command:

```
python pacman.py -l mediumMaze -p SearchAgent -a fn=ehc,heuristic=manhattanHeuristic
```

Other layouts are available in the [layouts](layouts/) directory, and you can easily create you own. When you use the `autograder` (see section [cheking submission](#checking-your-submission) ), it will try to validate your solution by looking for an exact match with your output. The successors list are expected to be visited in the original order given by the API. 

#### Part 2 (3 marks)

In this part we will help you prove to yourself that you have all the ingredients to pick up many new search algorithms, tapping to knowledge you acquired in the lectures and tutorials.

**Bidirectional  A\* Enhanced (BAE\*)** is a search algorithm that searches in both directions, from the intial state to the goal state, and from the goal state towards the initial state, and keeps track of solutions found when the two directions meet. In Part 2, for simplicity, we assume there is only one goal state and no food, hence, progression and regression both search in the same state space, one uses the transition function forward, and the other backward. This algorithm has not been introduced in the lectures but it relies on ingredients which you already know: 
*  **A\***,  
* the evaluation function <img src="https://latex.codecogs.com/svg.image?f(n)&space;=&space;g(n)&space;&plus;&space;h(n)"/> used to guide the expansion order in **A\***, and
* the definition of the transition function <img src="https://latex.codecogs.com/svg.image?f(s,a)" title="f(s,a)" /> to generate a graph implicitly while searching. 

**BAE\*** is similar to A*, but:
1. It has two open lists (lines 1-2, Alg. 1) to alternate (line 22) expanding nodes (line 9) from the forward <img src="https://latex.codecogs.com/svg.image?Open_f" title="Open_f" /> and backward <img src="https://latex.codecogs.com/svg.image?Open_b" /> lists.
2. While searcing, it keeps a lower bound <img src="https://latex.codecogs.com/svg.image?L" /> and upper bound <img src="https://latex.codecogs.com/svg.image?U" /> to know when to stop the search, i.e. when <img src="https://latex.codecogs.com/svg.image?L\geq&space;U" /> (line 15), the incumbent solution has been proved to be optimal. The lower bound is updated each time a node is selected for expansion (lines 5-9, Alg. 1), keeping track of the average <img src="https://latex.codecogs.com/svg.image?f(n)" /> value in both directions (line 8). Note that if the heuristic is **admissible**, then the minimum <img src="https://latex.codecogs.com/svg.image?f(n)" /> value in the open lists represent a lower bound on the optimal solution cost.
3. The upperbound is only updated if the current node <img src="https://latex.codecogs.com/svg.image?n" /> exists in the open list of the opossite direction <img src="https://latex.codecogs.com/svg.image?Open_{\bar{x}}" />, which means that the two directions meet through node <img src="https://latex.codecogs.com/svg.image?n" />, and (the cost of the path form the initial state to <img src="https://latex.codecogs.com/svg.image?n" />) + (the cost of the path from <img src="https://latex.codecogs.com/svg.image?n" /> to the initial state of the opposite direction) is smaller than the best solution known so far, i.e. <img src="https://latex.codecogs.com/svg.image?U" /> (line 11). If that's the case, keep the solution and update the upperbound (lines 12-13). 
4. The priority used to order nodes in the openlists is slightly different than A*. Given that every time a node is chosen for expansion we know that its <img src="https://latex.codecogs.com/svg.image?g_x(n)" /> value represents the optimal cost to node <img src="https://latex.codecogs.com/svg.image?n" /> in direction <img src="https://latex.codecogs.com/svg.image?x" />, then we can characterize and correct the innacuracy of the heuristic <img src="https://latex.codecogs.com/svg.image?h_{\bar{x}}" /> used in the opposite direction  as <img src="https://latex.codecogs.com/svg.image?d_x(n)&space;=&space;g_x(n)&space;-&space;h_{\bar{x}}" />. This value is added to <img src="https://latex.codecogs.com/svg.image?f_x(n')" /> and used to set the priority of each generated node <img src="https://latex.codecogs.com/svg.image?n'" /> (lines 18-20).

![Algorithm 1](img/alg1.png)

This algorithm is taken from the [paper](https://ojs.aaai.org/index.php/SOCS/article/view/21756/21520) presented at the [Symposium on Combinatorial Search](https://sites.google.com/unibs.it/socs2022/home?authuser=0), on the 22nd of July, 2022. Bidirectional search is currently a hot research topic given recent theoretical results and simple practical algorithms such as BAE*. The paper that introduced these results received a prestigious best paper award in a [top conference in AI](https://aip.riken.jp/award/aaai-20_honorable_mention/). This context alone should motivate you: you are literally implementing a cutting-edge search algorithm.

Tips for your implementation:

- Checking membership of a node in the opposite open (line 11), can be a computationally expensive operation (worst case linear in the size of the open, which in turn is exponential as the depth of the search increases). This is specially relevant as this line is executed for every expanded state. Think back about your Data Structures course, and use an auxiliary data structure to implement the membership test efficiently for this assignment. Otherwise, BAE* will be way slower than A*, even if it expands less nodes. 
- To understand what the search is doing, make sure that you understand the successor generators in both directions. In Backward search, given an action <img src="https://latex.codecogs.com/svg.image?a" /> and state <img src="https://latex.codecogs.com/svg.image?s" />, you need to generate the state <img src="https://latex.codecogs.com/svg.image?s'" /> from which the application of <img src="https://latex.codecogs.com/svg.image?a" /> resulted in state <img src="https://latex.codecogs.com/svg.image?s" />.
- The function `getBackwardsSuccessors` already reverses the action for backwards search.
- The function `getGoalStates` returns a list of all possible goal states.

Implement the **BAE\* algorithm** discussed above by inserting your code into the template indicated by comment ```***YOUR CODE HERE FOR TASK 2***```, you can view the location at this link: [search.py#L169](search.py#L169). You should be able to test the algorithm using the following command:
```
python pacman.py -l mediumMaze -p BidirectionalSearchAgent -a fn=bae,heuristic=manhattanHeuristic,backwardsHeuristic=backwardsManhattanHeuristic
```
Other layouts are available in the layouts directory, and you can easily create your own. The `autograder` will seek for exact match of the solution and the number of node expansions. The successors list are expected to be visited in the original order given by the API. If your node expansion number is incorrect, please try to **reverse** it. An example is given as follows:
```
succs = problem.getSuccessors(state)
succs.reverse()
```

#### Part 3 (4 marks)

This part involves solving a more complicated problem. You will be able to model the problem, using the **BAE\* algorithm** from part 2 and design a heuristic function that can guide the search algorithm. 

Just like in Q7 of the Berkerley Pacman framework, you will be required to create an agent that eats all the food (dots) in a maze. 

In order to implement this, you should create a new problem called `BidirectionalFoodSearchProblem`. Some of the variables are listed in the comments and the initialization. You will need to:

1. Design a way to represent the states of the problem. 
2. Return the initial state through `getStartState` function. 
3. Have `getGoalStates` to return a list of all possible goal states. 
4. Implement your transition function in `getSuccessors`, which should return a list of tuples that contains (`next state`, `action`, `cost`). 
5. Implement `getBackwardsSuccessors` function to search in backwards.
6. Implement two suitable heuristics, `bidirectionalCapsuleProblemHeuristic` and `bidirectionalCapsuleProblemBackwardsHeuristic`. 

Make sure your heuristic is admissible and consistent, as we don't check for reopening, and the optimality guarantees would be lost  because the assumption on the optimality of <img src="https://latex.codecogs.com/svg.image?g_x(n)" /> would be wrong when errors <img src="https://latex.codecogs.com/svg.image?d(n)" /> are computed. 

You may choose to implement other helper classes/functions. 

You should insert your code into the template indicated by the comments ```***YOUR CODE HERE FOR TASK 3***```, you can view the locations at these links:  9 tags in [searchAgents.py](searchAgents.py).

Tips for your implementation:
- It is important to make sure the transition from `getBackwardsSuccessors` is the correct reverse of `getSuccessors`. Forward and Backward are not equivalent as in Part 2.
- As there is not a single way to model this problem, we would not be checking node expansion numbers. However, please make sure that your code runtime is not too long. The `autograder` scripts will allow a time budget for each test case as 3 times the runtime of our staff's code with the blind heuristic that assigns each state a value of 0. 
- Although a heuristic function is not compulsory, however, a good heuristic function is going to improve your code's running time. We would recommend to try different heuristics to compare the running time and node expansion numbers. Start with the easiest one. 
- Running time for staff's code with zero heuristic on the trickySearch is **66 second** and on the mediumCorners is **30 seconds**.  This will give you a good guide for comparing the expected performance of your solution.
> **Note**
> We encourage you to test your submission on **new layouts and share them publicly** in this [ED Megathread](https://edstem.org/au/courses/10995/discussion/1219428) to enable other students to test their submissions on these layouts as well. You can start threads to discuss and compare performance with other students.

You should be able to test your program by running the following command (in one line):

```
python pacman.py -l smallCorners -p BidirectionalFoodSearchAgent -a fn=bae,heuristic=bidirectionalFoodProblemHeuristic,backwardsHeuristic=bidirectionalFoodProblemBackwardsHeuristic
```

The `autograder` seeks an optimal solution length within the time budget for each test cases. In addition, please make sure your heuristic is **admissible and consistent**, otherwise you should not assign full marks for this part due to not finding the optimal plan.


You will see in first person the balance between 1) how informed you make your heuristic (it should expand less nodes in general), and 2) the overall runtime. As you can see, sometimes it may be preferable to have a cheaper less informed heuristic, even if you end up expanding more nodes.

### Self Evaluation Task
You need to assign your marks for part 1 (3 marks), part 3 (3 marks) and part 3 (4 marks) based on your code performance and your own programming and learning experiences. 

Please fill in the self-evaluation section of the [SELFEV.md](SELFEV.md).

## Marking criteria

<!-- Marks are allocated according to the task breakdown listed above, based on how many of our tests the algorithms pass. No marks will be given for code formatting, etc.  -->
Marks are given based on your self evaluation. We are going to review your self evaluation and give you feedback about it, but we won't focus on the marks, rather on your qualitative evaluation reported in SELFEV.md.
 
You must **follow good SE practices**, including good use of git during your development such as:

* _Commit early, commit often:_ single or few commits with all the solution or big chucks of it, is not good practice.
* _Use meaningful commit messages:_ as a comment in your code, the message should clearly summarize what the commit is about. Messages like "fix", "work", "commit", "changes" are poor and do not help us understand what was done.
* _Use atomic commits:_ avoid commits doing many things; let alone one commit solving many questions of the project. Each commit should be about one (little but interesting) thing. 

## Checking your submission

<!-- **NOTE**: You should not change any files other than [search.py](search.py) and [searchAgents.py](searchAgents.py). You should not import any additional libraries into your code. This risks being incompatible with our running scripts. -->

> **Note**
> From this repository, we will copy *only* the files: [search.py](search.py) and [searchAgents.py](searchAgents.py) when testing the autograder in the server via tagging. Please do not change any other file as part of your solution, or it will not run in our server. 

Run the following command to run sanity checks using our test files:

```
python ./autograder.py --test-directory=test_cases_assignment1
```

It is important that you are able to run the autograder and have these tests pass, as this gives you valuable feedback about the validity of your solution.

> **Note**
> We encourage you to create and share your own test cases, you can create them following a similar styles as those we provided in [test_cases_assignment1/](test_cases_assignment1/). Please feel free to share your test cases in this [ED post](https://edstem.org/au/courses/10995/discussion/1219428)


## Docker Run
If you prefer not to set up your environment locally, you can run your code with docker. An example command for running the autograder is (please change the `bash` to `sh` if you are using a Windows PowerShell):
```
bash ./docker/docker_runner.sh python ./autograder.py --test-directory=test_cases_assignment1
```

You use similar command to test individual each individual part. However, docker does not support GUI, so please make sure you added `-t` option when test each individual part.
```
bash ./docker/docker_runner.sh python pacman.py -l mediumMaze -p SearchAgent -a fn=ehc,heuristic=manhattanHeuristic -t
```

## Submission Instructions

This repository serves as a start code for you to carry out your solution for [Project 1 - Search](http://ai.berkeley.edu/search.html) from the set of [UC Pacman Projects](http://ai.berkeley.edu/project_overview.html) and the marked questions. 

**To submit your assignment** you must complete the following **four** steps:


1. Check that your solution runs on Python 3.8 and that your source code does not include personal information, like your student number or name. 
2. Tag the commit that contains your final code with tag `submission`. 
    * The commit and tagging should be dated after the deadline.
    * Note that a tag is **NOT** a branch, so do not just create a branch called "submission" as that will not amount to tagging.
    * Note that a tag is **NOT** a commit message, so please make sure you can find it in your repo page -> tags
    * It is **case-sensitive**.
3. Complete the [SELFEV.md](SELFEV.md) file with your details of the submission.
4. **Make sure you fill in the [submission certification form](https://forms.gle/DxK3vAfsqW8LwEdz9)**.
<!-- 4. Fill the [Assignment 1 Certification Form](https://forms.gle/3W8ntjbW6Qq6NMvZA). -->

> **Warning**
>Non-certified submissions will attract **zero** marks.
    


<!-- From this repository, we will copy *only* the files: [search.py](search.py) and [searchAgents.py](searchAgents.py). Please do not change any other file as part of your solution, or it will not run. Breaking these instructions breaks our marking scripts, delays marks being returned, and more importantly, gives us a headache. Submissions not compatible with the instructions in this document will attract zero marks and do not warrant a re-submission. Staff will not debug or fix your submission. -->
The reason why we ask you to follow this process is to make sure you know how the final project/competition submission system will work. 

Please view the following to learn how to *Tag* your commit version you want to be graded:


### Git hints on tags:
**How to create a Tag using the Command Line**:


[![How to create a Tag the Command Line](img/loom_video.png)](https://www.loom.com/share/17ec72b954454bc89bbe1dbb0bd2874f)

**Another way to create a Tag using the User Interface**:

[![How to create a Tag the User Interface](img/loom_video.png)](https://www.loom.com/share/3cd39e97919e4b688d9841613aba6973)

## Important information

**Corrections:** From time to time, students or staff find errors (e.g., typos, unclear instructions, etc.) in the assignment specification. In that case, corrected version of this file will be produced, announced, and distributed for you to commit and push into your repository.  Because of that, you are NOT to modify this file in any way to avoid conflicts.

**Late submissions & extensions:** A penalty of 10% of the maximum mark per day will apply to late assignments up to a maximum of five days, and 100% penalty thereafter. Please include the late penalty in your mark if you do a late submission. Extensions will only be permitted in _exceptional_ circumstances; refer to [this question](https://docs.google.com/document/d/17YdTmDC54WHq0uZ-2UX3U8ESwULyBDJSD4SjKCrPXlA/edit?usp=sharing) in the course FAQs. 

**About this repo:** You must ALWAYS keep your fork **private** and **never share it** with anybody in or outside the course, except your teammates, _even after the course is completed_. You are not allowed to make another repository copy outside the provided GitHub Classroom without the written permission of the teaching staff. Please respect the [authors request](http://ai.berkeley.edu/project_instructions.html): 

> **_Please do not distribute or post solutions to any of the projects._**

**Academic Dishonesty:** This is an advanced course, so we expect full professionalism and ethical conduct.  Plagiarism is a serious issue. Please **don't let us down and risk our trust**. The staff take academic misconduct very seriously. Sophisticated _plagiarism detection_ software (e.g., [Codequiry](https://codequiry.com/), [Turinitin](https://www.turnitin.com/), etc.) will be used to check your code against other submissions in the class as well as resources available on the web for logical redundancy. These systems are really smart, so just do not risk it and keep professional. We trust you all to submit your own work only; please don't let us down.  If you do, we will pursue the strongest consequences available to us according to the **University Academic Integrity policy**. If you collaborate with other students, or use other materials, make sure to acknowledge it in the SELFEV.md document. For more information on this see file [Academic Integrity](ACADEMIC_INTEGRITY.md).

**We are here to help!:** We are here to help you! But we don't know you need help unless you tell us. We expect reasonable effort from you side, but if you get stuck or have doubts, please seek help. We will ran labs to support these projects, so use them! You can always ask general questions about the techniques that are required to solve the projects. If in doubt whether a questions is appropriate, post a Private post to the instructors.

**Silence Policy:** A silence policy will take effect **48 hours** before this assignment is due. This means that no question about this assignment will be answered, whether it is asked on the newsgroup, by email, or in person. Use the last 48 hours to wrap up and finish your project quietly as well as possible if you have not done so already. Remember it is not mandatory to do all perfect, try to cover as much as possible. By having some silence we reduce anxiety, last minute mistakes, and unreasonable expectations on others. 

Please remember to follow all the submission steps as per assignment specification.

## COMP90054 Code of Honour

We expect every UoM student taking this course to adhere to it **Code of Honour** under which every learner-student should:

* Submit their own original work (done individually or in their pair), or acknowledge the sources used.
* Do not share solutions with others, but provide instead insights to help others learn.
* Report suspected violations.
* Do not Engage in any other activities that will dishonestly improve their results or dishonestly improve or damage the results of others.

Unethical behaviour is extremely serious and consequences are painful for everyone. We expect enrolled students/learners to take full **ownership** of your work and **respect** the work of teachers and other students.


**I hope you enjoy the assignment and learn from it**, and if you still **have doubts about the assignment and/or this specification** do not hesitate asking in the [ED discussion Forum](https://edstem.org/au/courses/10995/discussion/) and we will try to address it as quickly as we can!

**GOOD LUCK and HAPPY PACMAN!**

## Acknowledgements

This is [Project 1 - Search](http://ai.berkeley.edu/search.html) from the set of [UC Pacman Projects](http://ai.berkeley.edu/project_overview.html).  We are very grateful to UC Berkeley CS188 for developing and sharing their system with us for teaching and learning purposes.
