# Youtube-video-growth
Final project for STATS102C (Acheived 4th Place out of 40 teams, All work was done in Rstudio then transferred to GitHub)

COMPETITION

YouTube is a large scale video-sharing platform owned by Google since late 2006. The site allows users to upload, view, rate, share, create playlists, comment on videos, and subscribe to other users. Of these interactions, views stand out as particularly important because they are a direct measure of engagement with a video and also help to determine how much revenue the content creator will make. A pressing goal for a content creator is to know how fast a video will grow, especially within the first few hours of its lifetime. A video’s early growth pattern can be a broader indicator of the eventual success of the video as well as the overall health of the channel.

In this project, we aim to predict the percentage change in views on a video between the second and sixth hour since its publishing. In order to predict this metric, we have several video features at our disposal, generally fitting into four categories explained below.

Thumbnail Image Features
Each video on YouTube has a thumbnail image, which is the first image that a user sees when deciding whether or not to click on the video. Content creators usually spend a great deal of time crafting the thumbnail image to be interesting, eye-catching, and representative of the content in the video. Several features in the data are extracted from the thumbnail image. Some features are more intuitive such as the average pixel value or the percent of nonzero pixels. Others such as the histogram of oriented gradient (HOG) features or convolutional neural network (CNN) features are harder to interpret but might give insights into the underlying structure of the thumbnail image.

Video Title Features
Another important factor in a video’s views is the title of the video, which a user also sees when deciding whether or not to watch a video. Content creators might try to craft the title to achieve various goals such as invoking certain emotions, asking a question to be answered, etc. Several features in the data are extracted from the video title. Some features are intuitive such as word count, character count and count of certain punctuation. Others, such as the Doc2Vec features, are harder to interpret but might give insights into the underlying semantic structure of the title text.

Channel Features
The third family of features relates not to an individual video but rather the channel which published the video. Indeed, an already very popular channel is likely to record different growth rates on a new video than a less popular channel, other factors held constant. This family of features consists of four features, each of which is measured as low, low_mid, mid_high, or high. For example, one feature is the number of subscribers that the channel has, as of June 8 2020. Another is the number of total views on all videos on the channel as of June 8, 2020.

Other Features
Other possibly useful features include the duration of the video, number of views after two hours since publishing, and the timestamp when the video was published.

Please see the 'Feature_Descriptions.xlsx' spreadsheet for full description of each feature.

REPORT

In addition to your score in this competition, your team will submit a report. At the end of the report, you should include a statement of contribution in which you describe the specific contributions of each team member to this project.

Your report must be a pdf file. Below, I provide guidelines on the components your report should at least have.

Introduction. Brief description of the problem. My advice is that you do not write a lengthy introduction, but just give the necessary components for your report. For instance, the goal of the problem. Another advice is that you avoid a report summary, since the grader will read the entire report anyway.

Methodology

Preprocessing. A description of the preprocessing of your data. For example, how you cleaned/transformed the data. If you decide to use advanced preprocessing techniques not discussed in class—for instance, oversampling and undersampling techniques, alternative coding schemes for categorical variables—you are expected to include references and a brief description of these techniques.
Statistical Model. A description of your final model accompanied by a good motivation of why you used this model. An example of a good motivation is to demonstrate that the model you chose outperformed all other alternative models you tried.
In this competition, you can use any data mining method or statistical model. Please refer to the Rules of this competition for more information on how to use methods not seen in class. If you use a method not seen in class, you are expected to give an explanation of the main features of the method as well as some references. If you use methods seen in class, you do not need to provide this information.
Results. Your best evaluation metric value with this model as shown in the Kaggle public leaderboard.

Conclusions. An explanation of why you think this model works well (or why you think it failed to work well.). You should also include ideas on how to improve or further improve the performance of your model.

Appendix. An Appendix showing your code. The code should be such that I can run it and get the exact same output as you. Ideally, the code is well-documented and organized.
