# fantasynba

I started getting into fantasy basketball in the 2019-2020 season, and began investigating methods which would maximize my chances of winning a league. Like many other beginners, I came across "punting". Punting is a strategy of completely giving up on one or more categories in a head to head category league in order to select players who maximize your team's strengths in other categories. If you don't know much about basketball, it's difficult to determine which punt cateogory or categories a specific player belongs to. Thus, I thought maybe I could use K-Means to cluster players into their respective punt categories.

## Dataset

I exported the rankings data from [Hashtag Basketball](https://hashtagbasketball.com/fantasy-basketball-rankings) into microsoft excel, applied some manual data cleaning, and simply loaded the data into R. The data can be found [here](https://github.com/oaarnikoivu/fantasynba/blob/master/rankings.xlsx).

## Cluster examples

### Studs

![Studs-stats](images/sx_stats.jpg)
![Studs-variance](images/sx_var.jpg)
![Studs2-stats](images/studs2.jpg)

### Centers

![centers](images/centers.jpg)

### Shooters

![shooters](images/shooters.jpg)
