import { AppPage } from './app.po';
import { browser, by, element } from 'protractor';
import { async, TestBed, ComponentFixture } from '@angular/core/testing';
import { ReactiveFormsModule, FormsModule } from "@angular/forms";
import { AuthComponent } from 'D:/workspace-sts-3.9.7.RELEASE/VariantDiag/VariantDiagWeb/src/app/auth/auth.component';
import 'zone.js/dist/zone.js';


describe('workspace-project App', () => {

	let component: AuthComponent;
  	let fixture: ComponentFixture<AuthComponent>;
  	
  	beforeEach(async(() => {
    	TestBed.configureTestingModule({
      		declarations: [ AuthComponent ]
    	})
    	.compileComponents();
  	}));

  	beforeEach(() => {
  		
  		TestBed.configureTestingModule({
      		declarations: [ AuthComponent ]
    	})
    	.compileComponents();
  	
  	
    	fixture = TestBed.createComponent(AuthComponent);
    	component = fixture.componentInstance;
    	fixture.detectChanges();
  	});
  	
  	it('should be created', () => {
    	expect(component).toBeTruthy();
  	});

	//it ("to check that text entered in text box displays on page",function() {
	//
	//	browser.get("https://www.joecolantonio.com/ProtractorExample.html");
	//	element(by.model("joeAngularText")).sendKeys("Joe Colantonio");
	//	
	//	element(by.binding("joeAngularText")).getText().then(function(text){
	//	
	//		console.log(text);
	//		
	//	});
	//	
	//});
	
	

	
	//it ("Authentification",function() {
	//
	//	browser.get("http://localhost:4200/#/tableaunormal/filtre");
	//	element(by.model('login')).sendKeys('DEV');
	//
	//});
	
});
