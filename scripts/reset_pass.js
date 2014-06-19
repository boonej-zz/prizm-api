var mongoose = require('mongoose');
var User = require('models/user').User;

User.find({email: 'testann@test.com'}, function(err, res){
  if(err) throw err;
  console.log(res);
});
