process.env.PRISM_HOME = process.cwd() + '/';
var Mail = require('classes/Mail');

var user = {
  email: 'test@test.com',
  first_name: 'U OF Awesome',
  enrollment: "250,000",
  mascot: 'Turtle',
  date_founded: '1482',
  address: '1623 Tubular Drive',
  state: 'California',
  city: 'City of The Dead',
  info: 'gemini, i like long walks on the beach',
  website: 'http://www.uofawesome.com'
};

/**var mail = new Mail();
mail.institutionReview(user);
**/
var config = require('config/default');
var mandrill = require('node-mandrill')(config.mandrill.dev.client_secret);

var MANDRILL_ENDPOINT_SEND = '/messages/send';

mandrill(MANDRILL_ENDPOINT_SEND, {
  message: {
    to: [{email: 'hayden.dj@gmail.com'}],
    from_email: 'admin@prizmapp.com',
    subject: 'Test email from mailchimp mandrill',
    html: '<h1>his is a test to ensure this works.</h1>'
  }
}, function(error, response){
  if(error) console.log('Error returned ' + JSON.stringify(error));

  else console.log(response);
});
