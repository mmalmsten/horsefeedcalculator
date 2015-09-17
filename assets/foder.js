var ws;
var hayekg = 0;
var hayprkg = 0;
var haycakg = 0;
var haypkg = 0;
var finalenergy = 0;
var finalprotein = 0;
var finalca = 0;
var finalp = 0;
var hayneedtwo = 0;


function ready(){
	if ('MozWebSocket' in window) {
		WebSocket = MozWebSocket;
	}
	if ('WebSocket' in window) {
		ws = new WebSocket('ws://' + window.location.host + '/foder');
		ws.onopen = function() {
			var erlang = document.getElementById('module').name
			ws.send('{\"pid\" : \"'+ erlang + '\",\"type\" : \"make\",\"values\" : []}');
		};
		ws.onmessage = function (evt) {
			if (evt.data) {
				var str = evt.data;
				eval(str);
				// var myObject = eval('(' + myJSONtext + ')');
			}
		};
		ws.onclose = function() {
			// websocket was closed
		};
	} else {
		// browser does not support websockets
	}
}

function calculate(){
		

	  	var brand_selector = document.getElementById("brand"); 
	  	var brand = brand_selector.options[brand_selector.selectedIndex].value;  // fodertillverkare

	  	var weight = document.getElementById("horseweight").value;  // hästens vikt

	  	var addoverweight = document.getElementById("addoverweight").value;  // hästens vikt

	  	var addunderweight = document.getElementById("addunderweight").value;  // hästens vikt

		var efweight = 0.5 * Math.pow((+weight + +addunderweight - +addoverweight), 0.75); // beräkna hästens MJ-behov
		
	  	var type = document.getElementById("horsetype").value; // hästens typ

		var basicenergy = (+efweight * +type).toFixed(1);  // hästens underhållsbehov MJ
		document.getElementById("basictotalenergy").value = basicenergy; 

		var basicprotein = (+basicenergy * 6).toFixed(1);  // hästens underhållsbehov SmbRp
		document.getElementById("basictotalprotein").value = basicprotein; 



	  	var walk = document.getElementById("horsewalk").value;  // så mycket hästen rids i skritt

	  	var trot = document.getElementById("horsetrot").value;  // så mycket hästen rids i trav eller galopp

	  	var week = document.getElementById("daysaweek").value;  // så ofta hästen rids per vecka

		var workenergy = ((((0.2 * (+weight / 100) * +walk) + (1.3 * (+weight / 100) * +trot)) / 7) * +week).toFixed(1);  // hästens arbetsbehov MJ
		document.getElementById("worktotalenergy").value = workenergy; 

		var workprotein = (+workenergy * 6).toFixed(1);  // hästens arbetsbehov SmbRp
		document.getElementById("worktotalprotein").value = workprotein; 



	  	var sex = document.getElementById("horsesex").value;  // tillägg om hingst

	  	var outdoors = document.getElementById("horseoutdoors").value;  // tillägg om grupphållning

	  	var age = document.getElementById("horseage").value; // tillägg om 20 år eller äldre

	  	var temp = document.getElementById("temperature").value; // tillägg om kyla
		
		
		var otherenergy = ((+sex * +basicenergy) + (+outdoors * +basicenergy) + (+age * +basicenergy) + (+temp * +basicenergy)).toFixed(1);  // hästens tilläggsbehov MJ
		document.getElementById("othertotalenergy").value = otherenergy; 

		var otherprotein = (((+sex * +basicenergy) + (+outdoors * +basicenergy) + (+age * +basicenergy)) * 6).toFixed(1);  // hästens tilläggsbehov SmbRp kyla ej inräknat
		document.getElementById("othertotalprotein").value = otherprotein; 



		var finalenergy = (+basicenergy + +workenergy + +otherenergy).toFixed(1);  // hästens totala behov av MJ
		document.getElementById("finaltotalenergy").value = finalenergy; 

		var finalprotein = (+basicprotein + +workprotein + +otherprotein).toFixed(1);  // hästens totala behov av SmbRp
		document.getElementById("finaltotalprotein").value = finalprotein; 

		var finalca = (+finalenergy * 0.4).toFixed(1);  // hästens totala behov av Ca samt Ca-behov om den arbetas
		if (finalenergy >= basicenergy * 1.25) {
			var finalca = (+finalenergy * 0.35).toFixed(1);
		}
		document.getElementById("finaltotalca").value = finalca; 

		var finalp = (+finalenergy * 0.25).toFixed(1);  // hästens totala behov av P
		document.getElementById("finaltotalp").value = finalp;
		
		
	  	var hayts = document.getElementById("hayanalysists").value;  // grovfoderanalysen ts

	  	var hayenergy = document.getElementById("hayanalysisenergy").value;  // grovfoderanalysen MJ
		
	  	var hayprotein = document.getElementById("hayanalysisprotein").value;  // grovfoderanalysen SmbRp

	  	var hayca = document.getElementById("hayanalysisca").value;  // grovfoderanalysen Ca

	  	var hayp = document.getElementById("hayanalysisp").value;  // grovfoderanalysen P		


	  	var hayekg = (+hayenergy * (+hayts / 100));  // grovfoderanalysen MJ per kg foder
		
	  	var hayprkg = (+hayprotein * (+hayts / 100));  // grovfoderanalysen SmbRp per kg foder

	  	var haycakg = (+hayca * (+hayts / 100));  // grovfoderanalysen Ca per kg foder

	  	var haypkg = (+hayp * (+hayts / 100));  // grovfoderanalysen P per kg foder
		



		var hayneed = ((+weight / 100) / (hayts / 100)).toFixed(1);  // hästens lägsta behov av grovfoder
		document.getElementById("hayneed").value = hayneed;

  	  	// täcks hästens lägsta behov av grovfoder?
	

		var hayneedtwo = (((+weight / 100) * 1.5) / (hayts / 100)).toFixed(1);  // hästens rekommenderat lägsta behov av grovfoder
		document.getElementById("hayneedtwo").value = hayneedtwo;
		
		// täcks hästens rekommenderat lägsta behov av grovfoder?


		// nuvarande smbrp/mj
		
		// nuvarande ca/p


ws.send('{\"pid\" : \"foder\",\"type\" : \"get\",\"values\" : ["' + brand + '",' + hayekg + ',' + hayprkg + ',' + haycakg + ',' + haypkg + ',' + finalenergy + ',' + finalprotein + ',' + finalca + ',' + finalp + ',' + hayneed + ',' + hayneedtwo + ']}');


		}		
		


function changebrand(brand){
	ws.send('{\"pid\" : \"manufacturer\",\"type\" : \"put\",\"values\" : ["' + brand + '"]}');
}


function chat(){
	var name = document.getElementById("chatname").value;
	var message = document.getElementById("chatmessage").value;

	function addZero(i) {
    if (i < 10) {
        i = "0" + i;
    }
    return i;
}
	var d = new Date();
	var date = d.getFullYear() + "-" + addZero(d.getMonth()) + "-" + addZero(d.getDay()) + " " + addZero(d.getHours()) + ":" + addZero(d.getMinutes());
	
	ws.send('{\"pid\" : \"chat\",\"type\" : \"post\",\"values\" : ["' + name + '","' + message +'","' + date +'"]}');
}

function updatechat(name,message,date){

var para = document.createElement("div");
var node = document.createTextNode(message);
para.appendChild(node);
var element = document.getElementById("newchatbox");
element.appendChild(para);

para.className = "bubble col-xs-7";
var para = document.createElement("div");

var node = document.createTextNode(name);
para.appendChild(node);
var element = document.getElementById("newchatbox");
element.appendChild(para);

var node = document.createElement("br");
para.appendChild(node);
var element = document.getElementById("newchatbox");
element.appendChild(para);

var node = document.createTextNode(date);
para.appendChild(node);
var element = document.getElementById("newchatbox");
element.appendChild(para);
para.className = "name col-xs-5";


var para = document.createElement("div");
var element = document.getElementById("newchatbox");
element.appendChild(para);
para.className = "clear";

var para = document.createElement("hr");
var element = document.getElementById("newchatbox");
element.appendChild(para);
para.className = "divide";

}


		
		function calcweight(){
		
		var gethorseweighta = document.getElementById("gethorseweighta").value;  // hästens viktmått A (lansmärkeomfång)

		var gethorseweightb = document.getElementById("gethorseweightb").value;  // hästens viktmått B (mankhöjd)

		var gethorseweightc = document.getElementById("gethorseweightc").value;  // hästens viktmått C ()

		var gethorseweightd = document.getElementById("gethorseweightd").value;  // hästens viktmått D ()

		var horseweight1 = ((4.3 * +gethorseweighta) + (3 * +gethorseweightb)).toFixed(1);  // beräkna hästens vikt

		var horseweight2 = (((+gethorseweightc * +gethorseweightc) * +gethorseweightd) / 8900).toFixed(1);  // beräkna hästens vikt
		
		var horseweight = ((+horseweight1 + +horseweight2) / 2).toFixed(1);
		
		document.getElementById("horseweight").value = horseweight;
}		


$(function() {
  $('a[href*=#]:not([href=#])').click(function() {
    if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'') && location.hostname == this.hostname) {
      var target = $(this.hash);
      target = target.length ? target : $('[name=' + this.hash.slice(1) +']');
      if (target.length) {
        $('html,body').animate({
          scrollTop: target.offset().top
        }, 1000);
        return false;
      }
    }
  });
});