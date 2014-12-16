var colors = ["#BA12FF", "#CC2713", "#25CC00"];
var numbers = [1,2,3];
var shading = ["solid","striped","open"];
var symbols = ["diamond","squiggle","oval"];

function card(color,number,shading,symbol){
    this.color = color;
    this.number = number;
    this.shading = shading;
    this.symbol = symbol;
};
