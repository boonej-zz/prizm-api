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

var mail = new Mail();
mail.institutionReview(user);

