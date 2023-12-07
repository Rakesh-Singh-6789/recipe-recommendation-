; Create templates
(deftemplate Recipe
   (slot name)
   (slot cuisine)
   (slot difficulty)
   (slot ingredients)
   (slot instructions)
   (slot rating)
)

(deftemplate User
   (slot name)
   (slot cuisine-preference)
   (slot difficulty-preference)
   (slot ingredient-preference)
   (slot allergy-preference)
)

(defrule TopRecipes 
    (declare (salience +1))
    =>
    (printout t "This is our highest rated recipe:" crlf)
    (bind ?rate 0)
    (bind ?recip nil)
    (do-for-all-facts ((?recipe Recipe)) TRUE
        (if (< ?rate ?recipe:rating)
            then
            (bind ?rate ?recipe:rating)
            (bind ?recip ?recipe:name)  
        )
    )
    (printout t " =*= " ?recip " =*= "crlf)
)

(defrule GetUserPreferences
    (not (User (name ?name)))
    =>
    ; inputs for user
    (printout t crlf)
    (printout t "insert your name !")
    (bind ?name (read))
    (printout t "Welcome, " ?name ". Let's find you a recipe!" crlf)
    (printout t "What cuisine do you prefer? ")
    (bind ?cuisine-pref (read))
    (printout t "How difficult should the recipe be? (Easy, Intermediate, Difficult) ")
    (bind ?difficulty-pref (read))
    (printout t "Do you have any specific ingredient preferences? ")
    (bind ?ingredient-pref (read))
    (printout t "Do you have any alergies? ")
    (bind ?allergy-pref (read))
    ; create new user fact
    (assert (User 
        (name ?name)
        (difficulty-preference ?difficulty-pref)
        (cuisine-preference ?cuisine-pref)
        (ingredient-preference ?ingredient-pref)
        (allergy-preference ?allergy-pref)
    ))
)


(deffunction RecommendRecipesByCuisine (?user)
    (bind ?cuisine-pref (fact-slot-value ?user cuisine-preference))
    (bind ?allergy-pref (fact-slot-value ?user allergy-preference))
    (printout t ?cuisine-pref " Cuisines: " crlf)
    (do-for-all-facts ((?recipe Recipe)) (and (eq ?recipe:cuisine ?cuisine-pref)
                                              (eq (str-index ?allergy-pref ?recipe:ingredients ) FALSE))
         (printout t " - " ?recipe:name crlf)
    )
)

(deffunction RecommendRecipesByIngredients (?user)
    (bind ?ingredient-pref (fact-slot-value ?user ingredient-preference))
    (bind ?allergy-pref (fact-slot-value ?user allergy-preference))
    (printout t "These recepies contains " ?ingredient-pref ":" crlf)
    (do-for-all-facts ((?recipe Recipe)) (and (neq (str-index ?ingredient-pref ?recipe:ingredients ) FALSE)
                                              (eq (str-index ?allergy-pref ?recipe:ingredients ) FALSE))
         (printout t " - " ?recipe:name crlf)
    )
)

(deffunction RecommendRecipesByDifficulty (?user)
    (bind ?difficulty-pref (fact-slot-value ?user difficulty-preference))
    (bind ?allergy-pref (fact-slot-value ?user allergy-preference))
    (printout t "These recepies " ?difficulty-pref " to prepare: "  crlf)
    (do-for-all-facts ((?recipe Recipe)) (and (eq ?recipe:difficulty ?difficulty-pref)
                                              (eq (str-index ?allergy-pref ?recipe:ingredients ) FALSE))
         (printout t " - " ?recipe:name crlf)
    )
)

(defrule PrintRecipeRecommendation
    ?user <- (User 
                (name ?name)
                (cuisine-preference ?cuisine-pref)
                (difficulty-preference ?difficulty-pref)
                (ingredient-preference ?ingredient-pref)
                (allergy-preference ?allergy-pref)
            )
    =>
    (bind ?check 0)
    (do-for-all-facts ((?recipe Recipe)) (and (eq ?recipe:cuisine ?cuisine-pref)
                                              (eq ?recipe:difficulty ?difficulty-pref)
                                              (neq (str-index ?ingredient-pref ?recipe:ingredients ) FALSE)
                                              (eq (str-index ?allergy-pref ?recipe:ingredients ) FALSE))
         (bind ?check (+ ?check 1))
        (printout t crlf)
        (printout t "Based on your preferences we suggest you cook these dishes:" crlf)
        (printout t "===== " ?recipe:name " =====" crlf)
        (printout t "ingredients: " ?recipe:ingredients crlf)
        (printout t "instructions: "?recipe:instructions crlf)
    )
    (if(eq ?check 0)
        then
        (printout t crlf)
        (printout t "Sorry we couldn't find a dish that suits your preferences," crlf)
        (printout t "Here are several other alternatives based on each preference parameter you provided." crlf)
        (printout t "(All of the following recipes are also free from your allergies)" crlf)

        (RecommendRecipesByCuisine ?user)
        (RecommendRecipesByIngredients ?user)
        (RecommendRecipesByDifficulty ?user)

    )
)

(defrule NonallergyRecepies
    ?user <- (User (name ?username) (allergy-preference ?allergy-pref))
    =>
    (printout t crlf)
    (printout t "Here are some other recipes that do not include " ?allergy-pref ":"crlf)
    (bind ?count 0)
    (do-for-all-facts ((?recipe Recipe)) (eq (str-index ?allergy-pref ?recipe:ingredients ) FALSE)
        (bind ?count (+ ?count 1))
        (printout t " - " ?recipe:name crlf)
    )
)

(defrule ExitRecommendation
   (User (name ?name))
   =>
   (printout t "Thank you for using the personalized recipe recommender, " ?name "!" crlf)
)

(deffacts SampleRecipes
    (Recipe 
        (name "Spaghetti Carbonara") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "spaghetti, eggs, bacon, Parmesan cheese") 
        (instructions "1. Cook spaghetti. 2. Fry bacon. 3. Mix eggs and cheese. 4. Toss with pasta.")
        (rating 4.5)
    )

    (Recipe 
        (name "Chicken Tikka Masala") 
        (cuisine Indian) 
        (difficulty Intermediate)
        (ingredients "chicken, yogurt, tomato sauce, spices") 
        (instructions "1. Marinate chicken. 2. Cook chicken. 3. Simmer in sauce.")
        (rating 4.2)
    )

    (Recipe 
        (name "Grilled Cheese Sandwich") 
        (cuisine American) 
        (difficulty Easy)
        (ingredients "bread, butter, cheese") 
        (instructions "1. Butter bread slices. 2. Add cheese between slices. 3. Grill until golden brown.")
        (rating 4.0)
    )

    (Recipe 
        (name "Caesar Salad") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "romaine lettuce, croutons, Parmesan cheese, Caesar dressing") 
        (instructions "1. Toss lettuce with dressing. 2. Add croutons and cheese.")
        (rating 4.3)
    )

    (Recipe 
        (name "Beef Stir-Fry") 
        (cuisine Chinese) 
        (difficulty Intermediate)
        (ingredients "beef, bell peppers, broccoli, soy sauce, garlic") 
        (instructions "1. Stir-fry beef and garlic. 2. Add vegetables and soy sauce.")
        (rating 4.4)
    )

    (Recipe 
        (name "Mushroom Risotto") 
        (cuisine Italian) 
        (difficulty Intermediate)
        (ingredients "rice, mushrooms, onion, white wine, chicken broth") 
        (instructions "1. Sauté onions and mushrooms. 2. Add rice and wine. 3. Gradually add broth.")
        (rating 4.6)
    )

    (Recipe 
        (name "Spicy Tofu Curry") 
        (cuisine Indian) 
        (difficulty Intermediate)
        (ingredients "tofu, coconut milk, curry paste, vegetables") 
        (instructions "1. Sauté tofu and vegetables. 2. Simmer in coconut milk and curry paste.")
        (rating 4.1)
    )

    (Recipe 
        (name "Fish Tacos") 
        (cuisine Mexican) 
        (difficulty Easy)
        (ingredients "fish fillets, tortillas, cabbage, salsa") 
        (instructions "1. Fry fish. 2. Assemble tacos with cabbage and salsa.")
        (rating 4.7)
    )

    (Recipe 
        (name "Pasta Primavera") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "pasta, mixed vegetables, olive oil, Parmesan cheese") 
        (instructions "1. Sauté vegetables. 2. Toss with cooked pasta and cheese.")
        (rating 4.3)
    )

    (Recipe 
        (name "Chicken Noodle Soup") 
        (cuisine American) 
        (difficulty Easy)
        (ingredients "chicken, noodles, carrots, celery, chicken broth") 
        (instructions "1. Cook chicken and vegetables in broth. 2. Add noodles.")
        (rating 4.5)
    )

    (Recipe 
        (name "Greek Salad") 
        (cuisine Greek) 
        (difficulty Easy)
        (ingredients "cucumbers, tomatoes, feta cheese, olives, olive oil") 
        (instructions "1. Combine vegetables, cheese, and olives. 2. Drizzle with olive oil.")
        (rating 4.2)
    )

    (Recipe 
        (name "Beef Tacos") 
        (cuisine Mexican) 
        (difficulty Easy)
        (ingredients "ground beef, taco seasoning, tortillas, lettuce, cheese") 
        (instructions "1. Brown beef with seasoning. 2. Assemble tacos with lettuce and cheese.")
        (rating 4.6)
    )

    (Recipe 
        (name "Shrimp Scampi") 
        (cuisine Italian) 
        (difficulty Intermediate)
        (ingredients "shrimp, garlic, white wine, butter, pasta") 
        (instructions "1. Sauté shrimp and garlic in butter. 2. Add white wine and serve over pasta.")
        (rating 4.4)
    )

    (Recipe 
        (name "Vegetable Curry") 
        (cuisine Indian) 
        (difficulty Easy)
        (ingredients "assorted vegetables, curry sauce, rice") 
        (instructions "1. Cook vegetables in curry sauce. 2. Serve over rice.")
        (rating 4.0)
    )

    (Recipe 
        (name "Caprese Salad") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "tomatoes, fresh mozzarella, basil, balsamic glaze") 
        (instructions "1. Arrange tomato and mozzarella slices. 2. Top with basil and drizzle with balsamic glaze.")
        (rating 4.5)
    )

    (Recipe 
        (name "Teriyaki Chicken") 
        (cuisine Japanese) 
        (difficulty Intermediate)
        (ingredients "chicken thighs, teriyaki sauce, broccoli, rice") 
        (instructions "1. Marinate chicken in teriyaki sauce. 2. Grill and serve with steamed broccoli and rice.")
        (rating 4.2)
    )

    (Recipe 
        (name "Pesto Pasta") 
        (cuisine Italian) 
        (difficulty Easy)
        (ingredients "pasta, basil pesto, cherry tomatoes, pine nuts") 
        (instructions "1. Toss cooked pasta with pesto. 2. Garnish with cherry tomatoes and pine nuts.")
        (rating 4.3)
    )

    (Recipe 
        (name "Beef and Broccoli Stir-Fry") 
        (cuisine Chinese) 
        (difficulty Intermediate)
        (ingredients "beef, broccoli, soy sauce, ginger, garlic") 
        (instructions "1. Stir-fry beef with ginger and garlic. 2. Add broccoli and soy sauce.")
        (rating 4.5)
    )

    (Recipe 
        (name "Chicken Fajitas") 
        (cuisine Mexican) 
        (difficulty Easy)
        (ingredients "chicken breasts, bell peppers, onions, fajita seasoning, tortillas") 
        (instructions "1. Sauté chicken, peppers, and onions with fajita seasoning. 2. Serve in tortillas.")
        (rating 4.4)
    )

    (Recipe 
        (name "Egg Fried Rice") 
        (cuisine Chinese) 
        (difficulty Easy)
        (ingredients "rice, eggs, peas, carrots, soy sauce") 
        (instructions "1. Scramble eggs and add to cooked rice with peas, carrots, and soy sauce.")
        (rating 4.1)
    )

    (Recipe 
        (name "Mango Salsa") 
        (cuisine Mexican) 
        (difficulty Easy)
        (ingredients "mangoes, red onion, cilantro, lime juice, jalapeño") 
        (instructions "1. Dice mangoes and mix with chopped onion, cilantro, lime juice, and jalapeño.")
        (rating 4.6)
    )
)
