import { HttpClient } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { Subject } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { Router } from '@angular/router';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit {
  
  liaison1																				;
  liaison2																				;
  liaison3																				;
  liaison4																				;
  liaison5																				;
  liaison6																				;
  liaison7																				;
  liaison8																				;
  liaison9																				;
  liaison10																				;
  liaison11																				;
  liaison12																				;
  liaison13																				;
  liaison14																				;
  liaison15																				;
  liaison16																				;
  liaison17																				;
  option																				;
  geneChoisie																			;
  exonChoisie																			;
  numberExonChoisie																		;
  canvas: HTMLCanvasElement																;
  tailleExon				= 0															;
  longeur					= 8															;
  limitecanvas 				= 1000														;
  largeurCanvas				= 1000														;
  hauteurCanvas				= 1000														;
  choixNumberExon 			= false														;
  choixOptionJunctionExon 	= false														;
  affichageC 				= false														;
  TableauOption				= 	[
  								['Cryptic exon inclusion'			, 'a'],
  							  	['Exon skipping'					, 'b'],
  							  	['Multiple exon skipping'			, 'c'],
  							  	['Splice intronic donor shift'		, 'd'],
  							  	['Splice intronic acceptor shift'	, 'e'],
  							  	['Splice exomic donor shift'		, 'f'],
  							  	['Splice exomic acceptor shift'		, 'g'],
  							  	]														;
  
  a 						= 120 + this.longeur										;
  
  leftcircle				= 120 														;
  leftcircle1				= 70  														;
  leftcircle2				= 163 														;
  leftcircle3				= 133														;
  leftcircle4				= 73														;
  leftcircle5				= 182														;
  leftcircle6				= (70 + this.longeur) + ((this.a - (70 + this.longeur))/2)	;
  leftcircle7				= 95 + this.longeur - 25									;
  leftcircle8				= 205														;
  leftcircle9				= 55 + this.longeur											;
  leftcircle10				= 85 + this.longeur - 25									;
  leftcircle11				= 75 + this.longeur											;
  leftcircle12				= 85 + this.longeur - 25									;
  leftcircle13				= 125 + this.longeur										;
  leftcircle14				= 120 + this.longeur - 25									;
  leftcircle15				= 65 + this.longeur											;
  leftcircle16				= 120 + this.longeur - 25									;
  
  circle					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle.toString()).concat('px')};
  circle1					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle1.toString()).concat('px')};
  circle2					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle2.toString()).concat('px')};
  circle3					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle3.toString()).concat('px')};
  circle4					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle4.toString()).concat('px')};
  circle5					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle5.toString()).concat('px')};
  circle6					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle6.toString()).concat('px')};
  circle7					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle7.toString()).concat('px')};
  circle8					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle8.toString()).concat('px')};
  circle9					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle9.toString()).concat('px')};
  circle10					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle10.toString()).concat('px')};
  circle11					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle11.toString()).concat('px')};
  circle12					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle12.toString()).concat('px')};
  circle13					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle13.toString()).concat('px')};
  circle14					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle14.toString()).concat('px')};
  circle15					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle15.toString()).concat('px')};
  circle16					= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle16.toString()).concat('px')};
  
  constructor() { }
  
  ngOnInit() { }

  onChangeGene() {
  	
  	this.choixNumberExon = true;
  	this.onChangeOption();
  	
  }
  
  onChangeExon() {
  console.debug('debut');
  	console.debug(this.circle);
  	console.debug(this.circle2);
  	if (this.exonChoisie.length == this.tailleExon) {
  	
  	} else if (this.exonChoisie.length == this.tailleExon + 1) {
  	
  		this.tailleExon = this.tailleExon 	+ 1;
  		this.longeur	= this.longeur 		+ 6;
  		
  		this.leftcircle		= this.leftcircle 	+ 6;
  		this.leftcircle1 	= this.leftcircle1 	+ 6;
  		this.leftcircle2 	= this.leftcircle2 	+ 6;
  		
  		this.leftcircle3	= this.leftcircle3 	+ 6 + 6/2;
  		this.leftcircle4 	= this.leftcircle4 	+ 6;
  		this.leftcircle5 	= this.leftcircle5 	+ 6 + 6;
  		
  		this.leftcircle9 	= this.leftcircle9 	+ 6;
  		this.leftcircle10 	= this.leftcircle10	+ 6;
  		
  		this.leftcircle11 	= this.leftcircle11 + 6;
  		this.leftcircle12 	= this.leftcircle12	+ 6;
  		
  		this.leftcircle13 	= this.leftcircle13 + 6 + 6;
  		this.leftcircle14 	= this.leftcircle14	+ 6 + 6/2;
  		
  		this.leftcircle15 	= this.leftcircle15 + 6;
  		this.leftcircle16 	= this.leftcircle16	+ 6 + 6/2;
  		
  		this.circle			= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle.toString()).concat('px')};
  		this.circle1		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle1.toString()).concat('px')};
  		this.circle2		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle2.toString()).concat('px')};
  		
  		this.circle3		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle3.toString()).concat('px')};
  		this.circle4		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle4.toString()).concat('px')};
  		this.circle5		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle5.toString()).concat('px')};
  		
  		this.circle9		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle9.toString()).concat('px')};
  		this.circle10		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle10.toString()).concat('px')};
  
  		this.circle11		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle11.toString()).concat('px')};
  		this.circle12		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle12.toString()).concat('px')};
  		
  		this.circle13		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle13.toString()).concat('px')};
  		this.circle14		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle14.toString()).concat('px')};
  		
  		this.circle15		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle15.toString()).concat('px')};
  		this.circle16		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle16.toString()).concat('px')};		
  		
  	} else if (this.exonChoisie.length == this.tailleExon - 1) {
  	
  		this.tailleExon = this.tailleExon 	- 1;
  		this.longeur	= this.longeur 		- 6;
  		
  		this.leftcircle		= this.leftcircle 	- 6;
  		this.leftcircle1 	= this.leftcircle1 	- 6;
  		this.leftcircle2 	= this.leftcircle2 	- 6;
  		
  		this.leftcircle3	= this.leftcircle3 	- 6 - 6/2;
  		this.leftcircle4 	= this.leftcircle4 	- 6;
  		this.leftcircle5 	= this.leftcircle5 	- 6 - 6;
  		
  		this.leftcircle9 	= this.leftcircle9 	- 6;
  		this.leftcircle10 	= this.leftcircle10	- 6;
  		
  		this.leftcircle11 	= this.leftcircle11 - 6;
  		this.leftcircle12 	= this.leftcircle12	- 6;
  		
  		this.leftcircle13 	= this.leftcircle13 - 6 - 6;
  		this.leftcircle14 	= this.leftcircle14	- 6 - 6/2;
  		
  		this.leftcircle15 	= this.leftcircle15 - 6;
  		this.leftcircle16 	= this.leftcircle16	- 6 - 6/2;
  		
  		this.circle			= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle.toString()).concat('px')};
  		this.circle1		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle1.toString()).concat('px')};
  		this.circle2		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle2.toString()).concat('px')};
  		
  		this.circle3		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle3.toString()).concat('px')};
  		this.circle4		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle4.toString()).concat('px')};
  		this.circle5		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle5.toString()).concat('px')};
  		
  		this.circle9		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle9.toString()).concat('px')};
  		this.circle10		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle10.toString()).concat('px')};
  		
  		this.circle11		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle11.toString()).concat('px')};
  		this.circle12		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle12.toString()).concat('px')};  		
  		
  		this.circle13		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle13.toString()).concat('px')};
  		this.circle14		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle14.toString()).concat('px')};
  		
  		this.circle15		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '110px', 'left': (this.leftcircle15.toString()).concat('px')};
  		this.circle16		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle16.toString()).concat('px')};		
  			
  	}
  	console.debug('fin');
  	console.debug(this.circle);
  	console.debug(this.circle2);
  	this.onChangeOption();
  	this.choixOptionJunctionExon = true;
  	
  }
  
  onChangeNumberExon() {
  	
  	this.onChangeOption();
  	
  }
  
  onChangeOption() {
  	
  	this.canvas  = document.querySelector('#canvas');
  	const context = this.canvas.getContext('2d');
  	context.clearRect(0, 0, this.limitecanvas, this.limitecanvas);
  	
  	if (this.option === 'a') {
  	
  		context.fillStyle = 'blue';
  		context.fillRect(20, 100, 50 + this.longeur, 25);

  		context.fillStyle = 'red';
  		context.fillRect(120 + this.longeur, 100, 37.5 , 25);

  		context.fillStyle = 'blue';
  		context.fillRect(207.5 + this.longeur, 100, 50 + this.longeur, 25);

  		context.beginPath();
  		context.moveTo(70 + this.longeur,112.5);
  		context.lineTo(120 + this.longeur, 112.5);

  		context.moveTo(157.5 + this.longeur,112.5);
  		context.lineTo(207.5 + this.longeur, 112.5);

  		context.moveTo(70 + this.longeur, 100);
  		context.lineTo(138.75 + this.longeur, 80);

  		context.moveTo(138.75 + this.longeur, 80);
  		context.lineTo(207.5 + this.longeur, 100);

  		context.moveTo(70 + this.longeur, 125);
  		context.lineTo(95 + this.longeur, 135);

  		context.moveTo(95 + this.longeur, 135);
  		context.lineTo(120 + this.longeur, 125);

  		context.moveTo(182.5 + this.longeur, 135);
  		context.lineTo(157.5 + this.longeur, 125);

  		context.moveTo(182.5 + this.longeur, 135);
  		context.lineTo(207.5 + this.longeur, 125);

  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText('CE', 130 + this.longeur, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()), 212 + this.longeur, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * ((this.liaison2 + this.liaison3) / 2) / this.liaison1).toString();
	  	
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200)

  		context.stroke();
  	
  	} else if (this.option === 'b') {
  		
  		var ajout = 12.5;
  		
  		context.fillStyle = 'blue';
  		context.fillRect(20, 100, 50 + this.longeur, 25);

  		context.fillStyle = 'red';
  		context.fillRect(120 + this.longeur, 100, 37.5 + ajout + this.longeur, 25);

  		context.fillStyle = 'blue';
  		context.fillRect(207.5 + ajout + this.longeur + this.longeur, 100, 50 + this.longeur, 25);

  		context.beginPath();
  		context.moveTo(70 + this.longeur,112.5);
  		context.lineTo(120 + this.longeur, 112.5);

  		context.moveTo(157.5 + this.longeur + this.longeur + ajout, 112.5);
  		context.lineTo(207.5 + this.longeur + this.longeur + ajout, 112.5);

  		context.moveTo(70 + this.longeur, 100);
  		context.lineTo(138.75 + ajout + this.longeur + this.longeur/2, 80);

  		context.moveTo(138.75 + ajout + this.longeur + this.longeur/2, 80);
  		context.lineTo(207.5 + this.longeur + this.longeur + ajout, 100);

  		context.moveTo(70 + this.longeur, 125);
  		context.lineTo(95 + this.longeur, 135);

  		context.moveTo(95 + this.longeur, 135);
  		context.lineTo(120 + this.longeur, 125);

  		context.moveTo(182.5 + this.longeur + this.longeur + ajout, 135);
  		context.lineTo(157.5 + this.longeur + this.longeur + ajout, 125);

  		context.moveTo(182.5 + this.longeur + this.longeur + ajout, 135);
  		context.lineTo(207.5 + this.longeur + this.longeur + ajout, 125);

  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()), 125 + this.longeur, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 2).toString()), 212 + this.longeur + this.longeur + ajout, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * ( this.liaison4 / ( ( this.liaison5 + this.liaison6 ) / 2 ) ) ).toString();
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200)

  		context.stroke();
  	
  	} else if (this.option === 'c') {
  		
  		if (typeof this.numberExonChoisie === 'undefined') {
  			
  			this.affichageC = false;
  		
  		} else if (this.numberExonChoisie < 1){
  		
  			context.clearRect(0, 0, 1000, 1000);
  		
  		} else {
  			
  			this.affichageC = true;
  			var ajout = 12.5;
  			
  			context.fillStyle = 'blue';
  			context.fillRect(20, 100, 50 + this.longeur, 25);
            this.a 	= 120 	+ this.longeur;
  			var b 	= 37.5 	+ ajout + this.longeur;
            context.beginPath();
            var exon = 'Exon ';
            context.font = '12px serif';
            
            for (var i = 1; i <= this.numberExonChoisie; i++) {
  			
  				context.fillStyle = 'red';
  				context.fillRect(this.a, 100, b, 25);
  				
  				context.moveTo(this.a + b, 112.5);
  				context.lineTo(this.a + b + 50, 112.5);
  				
  				context.moveTo(this.a + b, 125);
  				context.lineTo(this.a + b + 25, 135);
            
  				context.moveTo(this.a + b + 25, 135);
  				context.lineTo(this.a + b + 50, 125);
  				
  				context.fillStyle = 'white';
  				context.fillText(exon.concat((parseInt(this.exonChoisie) + i).toString()), this.a + 1, 116);
  				
  				this.leftcircle8	= this.a + b + 25 - 30,
            	this.circle8		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '155px', 'left': (this.leftcircle8.toString()).concat('px')};            
  				
  				this.a = this.a + b + 50;
            
            }
            
            this.leftcircle6 	= ((70 + this.longeur) + ((this.a - (70 + this.longeur))/2) - 30);
            this.circle6		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '120px', 'left': (this.leftcircle6.toString()).concat('px')};
            
            this.leftcircle7	= 95 + this.longeur - 30;
            this.circle7		= {'display': 'block', 'position': 'relative', 'height': '20px', 'width': '60px', 'top': '175px', 'left': (this.leftcircle7.toString()).concat('px')};
  
  			context.fillStyle = 'blue';
  			context.fillRect(this.a, 100, 50 + this.longeur, 25);
             			
  			context.moveTo(70 + this.longeur,112.5);
  			context.lineTo(120 + this.longeur, 112.5);
            
  			context.moveTo(70 + this.longeur, 100);
  			context.lineTo((70 + this.longeur) + ((this.a - (70 + this.longeur))/2), 80);
            
  			context.moveTo((70 + this.longeur) + ((this.a - (70 + this.longeur))/2), 80);
  			context.lineTo(this.a, 100);
            
  			context.moveTo(70 + this.longeur, 125);
  			context.lineTo(95 + this.longeur, 135);
            
  			context.moveTo(95 + this.longeur, 135);
  			context.lineTo(120 + this.longeur, 125);
            
  			context.fillStyle = 'white';
  			
  			context.fillText(exon.concat(this.exonChoisie), 25, 116);
            
	  		context.fillStyle = 'white';
	  		context.fillText(exon.concat((parseInt(this.exonChoisie) + i).toString()), this.a + 1, 116);
	  	    
	  		context.fillStyle = 'black';
	  		context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	    
	  		var pourcentage = 'NA';
	  		pourcentage = (100 * ( this.liaison7 / ( ( this.liaison8 + this.liaison9 ) / 2 ) ) ).toString();
	  		context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200)
            
  			context.stroke();
  			
  			if (this.a + b > this.limitecanvas) {
            	
            	this.limitecanvas 	= this.limitecanvas + 100;            	
            	this.canvas.width 	= this.limitecanvas;
            	this.onChangeOption();
            	            	
            }
  		
  		}
  	
  	} else if (this.option === 'd') {
    
    	context.fillStyle = 'blue';
  		context.fillRect(20, 100, 30 + this.longeur, 25);
  		
  		context.fillStyle = 'red';
  		context.fillRect(30 + this.longeur, 100, 20, 25);
  			
  		context.fillStyle = 'blue';
  		context.fillRect(20 + 50 + 50 + this.longeur, 100, 50 + this.longeur, 25);
  		
  		context.beginPath();
  		context.moveTo(20 + 30 + this.longeur, 112.5);
  		context.lineTo(20 + 50 + this.longeur + 50, 112.5);
  		
  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()), 125 + this.longeur, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 2).toString()), 212 + this.longeur + this.longeur + ajout, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * (this.liaison11 / this.liaison10)).toString();
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200);
	  	
	  	context.moveTo(50 + this.longeur, 125);
  		context.lineTo(85 + this.longeur, 135);

  		context.moveTo(85 + this.longeur, 135);
  		context.lineTo(120 + this.longeur, 125);
  		
  		context.moveTo(30 + this.longeur, 100);
  		context.lineTo(75 + this.longeur, 90);

  		context.moveTo(75 + this.longeur, 90);
  		context.lineTo(120 + this.longeur, 100);
  		
  		context.stroke();
  		
  	} else if (this.option === 'e') {
    
    	context.fillStyle = 'blue';
  		context.fillRect(20, 100, 50 + this.longeur, 25);
  		
  		context.fillStyle = 'red';
  		context.fillRect(20 + 50 + 50 + this.longeur, 100, 20, 25);
  			
  		context.fillStyle = 'blue';
  		context.fillRect(20 + 50 + 50 + this.longeur + 20, 100, 30 + this.longeur, 25);
  		
  		context.beginPath();
  		context.moveTo(20 + 50 + this.longeur, 112.5);
  		context.lineTo(20 + 50 + this.longeur + 50, 112.5);
  		
  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()), 125 + this.longeur, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 2).toString()), 212 + this.longeur + this.longeur + ajout, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * (this.liaison12 / this.liaison13)).toString();
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200);
	  	
	  	context.moveTo(20 + 50 + this.longeur, 125);
  		context.lineTo(85 + this.longeur, 135);

  		context.moveTo(85 + this.longeur, 135);
  		context.lineTo(120 + this.longeur, 125);
  		
  		context.moveTo(20  + 50 + this.longeur, 100);
  		context.lineTo(105 + this.longeur, 90);

  		context.moveTo(105 + this.longeur, 90);
  		context.lineTo(20  + 50 + 50 + 20 + this.longeur, 100);
  		
  		context.stroke();
  		
  	} else if (this.option === 'f') {
    
    	context.fillStyle = 'blue';
  		context.fillRect(20, 100, 50 + 50 + this.longeur + this.longeur, 25);
  			
  		context.fillStyle = 'blue';
  		context.fillRect(20 + 50 + 50 + this.longeur + 50 + this.longeur, 100, 50 + this.longeur, 25);
  		
  		context.beginPath();
  		context.moveTo(20 + 50 + 50 + this.longeur + this.longeur, 112.5);
  		context.lineTo(20 + 50 + 50 + this.longeur + this.longeur + 50, 112.5);
  		
  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie).concat('A'), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText(exon.concat(this.exonChoisie).concat('B'), 20 + 50 + this.longeur + 1, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()), 20 + 50 + 50 + this.longeur + 50 + this.longeur + 1, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * (this.liaison15 / this.liaison14)).toString();
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200);
	  	
	  	context.moveTo(20 + 50 + this.longeur, 125);
  		context.lineTo(20 + 50 + this.longeur + (50 + this.longeur + 50)/2, 135);

  		context.moveTo(20 + 50 + this.longeur + (50 + this.longeur + 50)/2, 135);
  		context.lineTo(20 + 50 + this.longeur + (50 + this.longeur + 50), 125);
  		
  		context.moveTo(20  + 50 + 50 + this.longeur + this.longeur, 100);
  		context.lineTo(20  + 50 + 50 + this.longeur + this.longeur + 50/2, 90);
  		
  		context.moveTo(20  + 50 + 50 + this.longeur + this.longeur + 50/2, 90);
  		context.lineTo(20  + 50 + 50 + this.longeur + this.longeur + 50, 100);

  		context.moveTo(20 + 50 + this.longeur, 100);
  		context.lineTo(20 + 50 + this.longeur, 125);
  		
  		context.stroke();
  		
  	} else if (this.option === 'g') {
    
    	context.fillStyle = 'blue';
  		context.fillRect(20, 100, 50 + this.longeur, 25);
  			
  		context.fillStyle = 'blue';
  		context.fillRect(20 + 50 + 50 + this.longeur, 100, 50 + 50 + this.longeur + this.longeur, 25);
  		
  		context.beginPath();
  		context.moveTo(20 + 50 + this.longeur, 112.5);
  		context.lineTo(20 + 50 + this.longeur + 50, 112.5);
  		
  		context.font = '12px serif';
  		context.fillStyle = 'white';
  		var exon = 'Exon ';
  		context.fillText(exon.concat(this.exonChoisie), 25, 116);

  		context.fillStyle = 'white';
  		context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()).concat('A'), 20 + 50 + 50 + this.longeur + 1, 116);

	  	context.fillStyle = 'white';
	  	context.fillText(exon.concat((parseInt(this.exonChoisie) + 1).toString()).concat('B'), 20 + 50 + 50 + this.longeur + 50 + this.longeur + 1, 116);
	  	
	  	context.fillStyle = 'black';
	  	context.fillText(this.geneChoisie, 125 + this.longeur, 170);
	  	
	  	var pourcentage = 'NA';
	  	pourcentage = (100 * (this.liaison17 / this.liaison16)).toString();
	  	context.fillText("This anomaly is present at a percentage of " + pourcentage + "%", 20, 200);
	  	
	  	context.moveTo(20 + 50 + this.longeur, 125);
  		context.lineTo(20 + 50 + this.longeur + (50 + this.longeur + 50)/2, 135);

  		context.moveTo(20 + 50 + this.longeur + (50 + this.longeur + 50)/2, 135);
  		context.lineTo(20 + 50 + this.longeur + (50 + this.longeur + 50), 125);
  		
  		context.moveTo(20  + 50 + this.longeur, 100);
  		context.lineTo(20  + 50 + this.longeur + 50/2, 90);
  		
  		context.moveTo(20  + 50 + this.longeur + 50/2, 90);
  		context.lineTo(20  + 50 + this.longeur + 50, 100);

  		context.moveTo(20 + 50 + this.longeur + 50 + 50 + this.longeur, 100);
  		context.lineTo(20 + 50 + this.longeur + 50 + 50 + this.longeur, 125);
  		
  		context.stroke();
  		
  	}
  
  }
  
  onChangeliaison1()	 { this.onChangeOption(); }
  onChangeliaison2()	 { this.onChangeOption(); } 
  onChangeliaison3()	 { this.onChangeOption(); }
  onChangeliaison4()	 { this.onChangeOption(); }
  onChangeliaison5()	 { this.onChangeOption(); } 
  onChangeliaison6()	 { this.onChangeOption(); }
  onChangeliaison7()	 { this.onChangeOption(); }
  onChangeliaison8()	 { this.onChangeOption(); } 
  onChangeliaison9()	 { this.onChangeOption(); }
  onChangeliaison10()	 { this.onChangeOption(); }
  onChangeliaison11()	 { this.onChangeOption(); } 
  onChangeliaison12()	 { this.onChangeOption(); }
  onChangeliaison13()	 { this.onChangeOption(); }
  onChangeliaison14()	 { this.onChangeOption(); } 
  onChangeliaison15()	 { this.onChangeOption(); }
  onChangeliaison16()	 { this.onChangeOption(); } 
  onChangeliaison17()	 { this.onChangeOption(); }

}
  