const exercises = document.querySelectorAll(".box-exercise");

exercises.forEach(exercise => {
    const btn = exercise.querySelector(".btn-show-answers");
    const answers = exercise.querySelectorAll(".answer");
    const solution = exercise.querySelector(".solution");

    btn.addEventListener("click", () => {
        for (answer of answers) {
            if (answer.value === "") {
                alert("You should try answering the exercises before seeing the answers.");
                return 0;
            }
        }

        if (solution.style.display === "") {
            solution.style.display = "block";
            btn.textContent = "Hide answers";
        }
        else {
            solution.style.display = "";
            btn.textContent = "Show answers";
        }
    });

});