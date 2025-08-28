data= read.csv("C:\\Users\\abhik\\Desktop\\STATISTICS\\Projects\\The Anatomy of a Goal\\events.csv")
dim(data)
head(data)

#dropping unnecessary columns
df <- data[ , !(names(data) %in% c("id_odsp", "id_event","sort_order","text","event_type
","event_type","event_type2","player2", "player_in", "player_out"))]

#ommitting events which did not lead to a shot
df=na.omit(df)

str(df)

# Load libraries
library(pROC)
library(patchwork)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(caret)   # for confusionMatrix etc.
library(janitor) # for tabyl (clean frequency tables)


#factoring variables
df <- df %>%
  mutate(
    
    # Side (home/away)
    side = factor(side,
                  levels = c(1, 2),
                  labels = c("Home", "Away")),
    
    # Body part
    bodypart = factor(bodypart,
                      levels = c(1, 2, 3),
                      labels = c("Right Foot", "Left Foot", "Head")),
    
    # Assist method
    assist_method = factor(assist_method,
                           levels = c(0, 1, 2, 3, 4),
                           labels = c("None", "Pass", "Cross", "Headed Pass", "Through Ball")),
    
    # Situation
    situation = factor(situation,
                       levels = c(1, 2, 3, 4),
                       labels = c("Open Play", "Set Piece", "Corner", "Free Kick"))
  )




# Plot class distribution
p1=ggplot(df, aes(factor(is_goal))) +
  geom_bar(fill="steelblue") +
  labs(title="Distribution of Goals vs No Goals", x="Is Goal", y="Count")


# Match minute
p2=ggplot(df, aes(time)) +
  geom_histogram(binwidth=5, fill="purple", color="black") +
  labs(title="Distribution of Match Minutes for Shots", x="Minute", y="Count")

# Location
p3=ggplot(df, aes(factor(location))) +
  geom_bar(fill="darkgreen") +
  coord_flip() +  # easier to read categories
  labs(title="Shot Location Distribution", x="Location", y="Count")

# Bodypart
p4=ggplot(df, aes(factor(bodypart))) +
  geom_bar(fill="orange") +
  labs(title="Body Part Distribution", x="Body Part", y="Count")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


#bivariate analysis
# Conversion rate by location
p1=df %>%
  group_by(location) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(location), y=goal_rate)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Goal Conversion Rate by Location", x="Location", y="Conversion Rate")

# Bodypart vs goal
p2=df %>%
  group_by(bodypart) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(bodypart), y=goal_rate)) +
  geom_col(fill="tomato") +
  labs(title="Goal Conversion Rate by Body Part", x="Body Part", y="Conversion Rate")

# situation vs goal
p3=df %>%
  group_by(situation) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(situation), y=goal_rate)) +
  geom_col(fill="gold") +
  labs(title="Goal Conversion Rate by Situation", x="Situation", y="Conversion Rate")

# Assist vs goal
p4=df %>%
  group_by(assist_method) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(assist_method), y=goal_rate)) +
  geom_col(fill="skyblue") +
  labs(title="Goal Conversion Rate by Assist Method", x="Assist Method", y="Conversion Rate")

grid.arrange(p1, p2,p3,p4, nrow = 2, ncol = 2)



#Mulitvariate analysis
# Location x Bodypart
p1=df %>%
  group_by(location, bodypart) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(location), y=goal_rate, fill=factor(bodypart))) +
  geom_col(position="dodge") +
  coord_flip() +
  labs(title="Goal Rate by Location and Body Part", x="Location", y="Conversion Rate")

# Situation x Assist Method
p2=df %>%
  group_by(situation, assist_method) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(situation), y=goal_rate, fill=factor(assist_method))) +
  geom_col(position="dodge") +
  labs(title="Goal Rate by Situation and Assist Method", x="Situation", y="Conversion Rate")

# Situation x Body part
p3=df %>%
  group_by(situation, bodypart) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(situation), y=goal_rate, fill=factor(bodypart))) +
  geom_col(position="dodge") +
  labs(title="Goal Rate by Situation and Bodypart", x="Situation", y="Conversion Rate")

#Location x Assist Method
p4=df %>%
  group_by(location, assist_method) %>%
  summarise(goal_rate = mean(is_goal)) %>%
  ggplot(aes(x=factor(location), y=goal_rate, fill=factor(assist_method))) +
  geom_col(position="dodge") +
  labs(title="Goal Rate by Location and Assist Method", x="Location", y="Conversion Rate")


grid.arrange(p1, p2,p3,p4, nrow = 2, ncol = 2)


#player conversion rate
player_conversion <- df %>%
  group_by(player) %>%
  summarise(
    total_shots = n(),
    goals = sum(is_goal),
    conversion_rate = mean(is_goal)
  ) %>%
  filter(total_shots >= 450) %>%
  arrange(desc(conversion_rate))

print(player_conversion, n=Inf)


player_conversion %>%
  arrange(desc(conversion_rate)) %>%
  ggplot(aes(x = reorder(player, conversion_rate), 
             y = conversion_rate, fill = conversion_rate)) +
  geom_col() +
  coord_flip() +  # Flip for readability
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  labs(title = "Goal Conversion Rate of Elite Players (min 450 shots)",
       x = "Player",
       y = "Conversion Rate") +
  theme_minimal(base_size = 13)


#get the elite player names
elite_players <- player_conversion$player

# Add dummy column 'elite' to the main dataset
df <- df %>%
  mutate(elite = if_else(player %in% elite_players, 1, 0))


#team conversion rate
team_conversion <- df %>%
  group_by(event_team) %>%
  summarise(
    total_shots = n(),
    goals = sum(is_goal),
    conversion_rate = mean(is_goal)
  ) %>%
  filter(total_shots >= 1000) %>%
  arrange(desc(conversion_rate))

print(team_conversion, n=20)


team_conversion %>%
  arrange(desc(conversion_rate)) %>%
  slice_head(n = 20)%>%
  ggplot(aes(x = reorder(event_team, conversion_rate), 
             y = conversion_rate, fill = conversion_rate)) +
  geom_col() +
  coord_flip() +  # Flip for readability
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  labs(title = "Goal Conversion Rate of Teams (min 1000 shots)",
       x = "Team",
       y = "Conversion Rate") +
  theme_minimal(base_size = 13)


#player conversion rate across factors
p1=df %>%
  filter(elite == 1) %>%
  group_by(bodypart,player) %>%
  summarise(conv = mean(is_goal), .groups="drop") %>%
  ggplot(aes(x=player, y=conv, fill=bodypart)) +
  coord_flip() +
  geom_col(position="dodge") +
  labs(title="Elite Players: Conversion by Body Part", 
       x="Player", y="Conversion Rate") +
  theme_minimal()

p2=df %>%
  filter(elite == 1) %>%
  group_by(situation,player) %>%
  summarise(conv = mean(is_goal), .groups="drop") %>%
  ggplot(aes(x=player, y=conv, fill=situation)) +
  coord_flip() +
  geom_col(position="dodge") +
  labs(title="Elite Players: Conversion by Situation", 
       x="Player", y="Conversion Rate") +
  theme_minimal()

p3=df %>%
  filter(elite == 1) %>%
  group_by(assist_method,player) %>%
  summarise(conv = mean(is_goal), .groups="drop") %>%
  ggplot(aes(x=player, y=conv, fill=assist_method)) +
  coord_flip() +
  geom_col(position="dodge") +
  labs(title="Elite Players: Conversion by Assist Method", 
       x="Player", y="Conversion Rate") +
  theme_minimal()

p4=df %>%
  filter(elite == 1) %>%
  group_by(side,player) %>%
  summarise(conv = mean(is_goal), .groups="drop") %>%
  ggplot(aes(x=player, y=conv, fill=side)) +
  coord_flip() +
  geom_col(position="dodge") +
  labs(title="Elite Players: Conversion by Home vs Away", 
       x="Player", y="Conversion Rate") +
  theme_minimal()

grid.arrange(p1, p2,p3,p4 ,nrow = 4, ncol = 1)



#logistic regression
elite_players <- player_conversion$player
df$elite <- ifelse(df$player %in% elite_players, "Yes", "No")
df$elite <- factor(df$elite, levels = c("Yes", "No"))

df <- df %>%
  mutate(
    location = factor(location),
  )

# Logistic regression model
logistic <- glm(is_goal ~ location + bodypart + situation + assist_method + side + time+elite,
             data = df, family = binomial)

summary(logistic)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
 
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
 
## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
 


set.seed(123)
trainIndex <- createDataPartition(df$is_goal, p = 0.7, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

# Logistic regression model
model <- glm(is_goal ~ location + bodypart + situation + assist_method + side + time+elite,
             data = train, family = binomial)

summary(model)

# Predict probabilities on test set
test$prob <- predict(model, newdata = test, type = "response")

# Convert probabilities to class predictions (0.5 threshold)
test$pred <- ifelse(test$prob > 0.5, 1, 0)

test$pred <- factor(test$pred, levels = c(0,1), labels = c("No","Yes"))
test$is_goal <- factor(test$is_goal, levels = c(0,1), labels = c("No","Yes"))

confusionMatrix(test$pred, test$is_goal, positive = "Yes")

roc_obj <- roc(test$is_goal, test$prob)
auc(roc_obj)

#Plot ROC
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for Goal Prediction")
abline(a = 0, b = 1, lty = 2, col = "gray") # reference line



