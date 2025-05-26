document.addEventListener('DOMContentLoaded', () => {
    const dino = document.querySelector('.dino');
    const grid = document.querySelector('.grid');
    const alert = document.getElementById('alert')
    
    let isJumping = false;
    let t = 0;
    let isGameOver = false;
    let pos = 0;
    
    

    function control(e){
         if (e.code === "Space" && isJumping == false)
         {
            console.log('Jump');
            jump(); 
            

            console.log('dino px: ', dino.style.bottom)
         }


        
    }

    let position = 0;
    function jump(){
        isJumping = true;
        let counter = 0;
        let velocity0 = 140;
        let gravity = 70;
        let timerId = setInterval(() => {

            //move down
            if (counter === 41) {
                clearInterval(timerId);
                isJumping = false;
               
            }

            
            else
            {
            //move up
            pos = velocity0 * 0.1* counter - (0.5 * gravity * Math.pow(0.1*counter,2));
       
            dino.style.bottom = pos + 'px';
            counter++;
            console.log('position y: ', + counter + ' =>' +   dino.style.bottom)
            }
        },20)
    }
    
    function generateObstacles(){
        if(!isGameOver){
        let randomTime = Math.random()*4000;
        let obstaclePosition = 1000
        const obstacle = document.createElement('div')
        obstacle.classList.add('obstacle')
        grid.appendChild(obstacle)
        obstacle.style.left = obstaclePosition + 'px'

        let timerId = setInterval(function(){
            obstaclePosition -=10;
            obstacle.style.left = obstaclePosition + 'px'

            if(obstaclePosition > 0 && obstaclePosition < 60 && pos < 60)
            {
                clearInterval(timerId)
                alert.innerHTML = 'Game Over'
                isGameOver = true;
                //remove all children
                while(grid.firstChild)
                {
                    grid.removeChild(grid.lastChild)
                }
            }

            
        },20)
        setTimeout(generateObstacles, randomTime)
        }
    }

    generateObstacles(); 




    document.addEventListener('keydown', control);
})