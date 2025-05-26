function createButton()
{   
    let zone = document.getElementById('NewButtonZone');
    zone.innerHTML = ''
    alert("Input value: " + buttonCounter.value);    
    

    for(let i = 1; i <= buttonCounter.value; i++)
    {
        let newButton = document.createElement('button')
        newButton.id = `Button Nr: ${i}`
        zone.appendChild(newButton);
        newButton.innerText = "New Button Nr: " + (i)
        newButton.style = "width: 100%; margin-left: auto; margin-right: auto;"
        newButton.onclick = function(e){
            let button = e.target;
            alert("Hello " + button.id)
        }
    }    
}