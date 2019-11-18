import { browser, by, element } from 'protractor';

export class AppPage {
  navigateTo() {
    return browser.get('http://localhost:4200/#/authentification');
  }
  
  login() {
  	element(by.model('login')).sendKeys('DEV');
  }
  
  getlogin() {
  	return element(by.binding('login')).getText();
  }

  getParagraphText() {
    return element(by.css('app-root h1')).getText();
  }
}
