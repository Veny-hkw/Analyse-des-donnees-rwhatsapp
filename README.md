README : Data Analysis : whatsapp conversations

The purpose of this project is to analyze data from hatsapp social media. We want to show relation, pattern, etc. in data.

The data used in this project come from my whatsapp chats.

The repository contains the following files :

README.md : provides an overview of the project.

CodeBook.md : describes the contents of the data set (data, names of variables, transformation over data set).

raw_data.txt : show raw data structure.

Analysis.r : the R script containing all code.

clean_dataset.txt : the data output (the file containing final data set).

html document : show all the project without code.

Informations over data set :

The raw data set has five (5) variables, such as :

time : variable time makes the chronology of all conversations since the beginning.

author : show all group users or members. Here, we have four (4) users, Parker, Elbert, Denson and Veny.

text : contains all chats or messages send by all users. Text variable contains : text, emojis, and others characters.

source : returns the name of the file that contains all our project.

emoji : emoji variable is nest so, instead to see emojis, we see NULL.

emoji_name : describes the emojis names. emoji_name variable is nest so, instead to see emojis, we see NULL.

here is image : raw data image

![](Capture%20d’écran%202024-07-15%20235100.png)

Clean the data set :

To clean the raw data set, we have made some transformation.

At first, we remove NA and every character string whose we don't need in the data processing.

Secondly, we remove punctuation, numbers, whitespace, etc.

Thirdly, we clean names of users. sometimes, whatsapp users have complex names.

For example : peter's whatsapp profil username is [dEUS\@Godisgood](mailto:dEUS@Godisgood){.email}. For a best visibility, we need to replace [dEUS\@Godisgood](mailto:dEUS@Godisgood){.email} by peter.

Finally, we diselect two variables, in that specific event variables source and time. And i add two new variables, temps and saison.

Temps is in date format. And Saison is a time sequence of six (6) month.

here is image : clean data image

![](Capture%20d’écran%202024-07-16%20002430.png)
