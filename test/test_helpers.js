var _prism_home   = process.env.PRISM_HOME,
    _auth_models  = require(_prism_home + 'models/auth'),
    _utils        = require(_prism_home + 'utils'),
    Token         = _auth_models.Token,
    Client        = _auth_models.ClientApplication,
    Code          = _auth_models.Code,
    User          = require(_prism_home + 'models/user').User;

exports.fetchAuthHeader = function(id, secret){
  return "Basic " + (new Buffer(id + ':' + secret).toString('base64'));
};

exports.createTestClient = function(callback){
	var test = new Client({
      name: 'AuthTest',
      description: 'Here is another desc',
      redirect_uri: 'https://localhost:3000/callback'
    }).save(function(error, client){
      if(error) throw error;
      callback(client);
    });
};

exports.destroyTestClient = function(callback){
	Client.remove({}, function(err){
    if(err) throw err;
    callback();
  });
};

exports.createTestCode = function(callback){
	var newCode = new Code({
    client_id: client.client_id,
    redirect_uri: client.redirect_uri
  }).save(function(error, code){
    if(error) throw error;
    callback(code);
  });
};

exports.destroyTestCode = function(callback){
	Code.remove({}, function(err){
    if(err) throw err;
    callback();
  });
};

exports.fetchFakeUsersArray = function(){
  var users = [
    {first_name: 'mark', last_name: 'zuckerberg', email: 'don.juan@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRYi9HtZArUiI9FeoRfuITjXZ1fQWg5W9fK6gimm02MwaRnGneSdK8Cc_pivpeLktVye6U'},
    {first_name: 'edwardo', last_name: 'saverin', email: 'don.juan1@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcRgH-YDfVCRISQlMmmV14Ufajqad3-Nt3HillHP56iOUCuLg3rxaO7nQ5okbBQF_5acMQE'},
    {first_name: 'cameron', last_name: 'winklevoss', email: 'don.juan2@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcTfpTtJLqJ8iWq4HhiMEmbrA42GPUNdVCLX0tviiJ9UQ-xQ7gRgv0aQD1we6oALpUxyYBA'},
    {first_name: 'erica', last_name: 'albright', email: 'don.juan4@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcThtTCT3PV3toz1Hp5DhpqAeAWpuZOETwkoXw7ZL6QJ-bgTq2nzKNV1XQPDGuGQgoM-iQM'},
    {first_name: 'sean', last_name: 'parker', email: 'don.juan5@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn2.gstatic.com/images?q=tbn:ANd9GcSbsO8l6rTC-yIwFys2c7dSQ4HJlOBQmtedvC6R2dQBQp88RVKVzyR_a593IsGkklreG2k'},
    {first_name: 'maryolin', last_name: 'deploy', email: 'don.juan6@awesome.com', password: 'pass123', profile_photo_url: 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRfBCpd7yf1VL-k-GLDFrf7mx0EE1YHYEqoZL6EymNeaajL9tqR9_CFrU6LhL5oJcK_TVc'}
  ];
  return users;
};

exports.fetchFakePostsArray = function(testUser, user){
  var posts = [
            {text: 'test test tes1', creator: testUser._id, target_id: user._id, category: 'experience', create_date: Date.now() - 10 * 60 * 1000},
            {text: 'test test tes2', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() - 60 * 60 * 100},
            {text: 'test test tes3', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 60 * 60 * 1000},
            {text: 'test test tes4', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 1 * 60 * 60 * 1000},
            {text: 'test test tes5', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 45 * 60 * 1000},
            {text: 'test test tes6', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 10 * 60 * 1000},
            {text: 'test test tes7', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 2 * 10 * 60 * 1000},
            {text: 'test test tes8', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() - 1 * 60 * 60 * 1000},
            {text: 'test test tes9', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 10 * 60 * 60 * 1000},
            {text: 'test test tes10', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() - 10 * 60 * 60 * 1000},
            {text: 'test test tes11', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 30 * 60 * 1000},
            {text: 'test test tes12', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 15 * 60 * 1000},
            {text: 'test test tes13', creator: testUser._id, target_id: user._id, category: 'experience',create_date: Date.now() + 10 * 60 * 100}
          ];
  return posts;
};

exports.createTestToken = function(callback){
	var testClient = new Client({
		name: "test",
		description: "description",
		redirect_uri: "https://localhost:3000/callback"
	});
  testClient.save(function(error, client){
    if(error) throw error;
    var testCode = new Code({client_id: client.client_id});
    testCode.save(function(error, code){
      if(error) throw error;
      var testToken = new Token({code: code.code, client_application: client, grant_type: 'authorization_code'})
      .save(function(error, token){
        if(error) throw error;
        callback(token, testCode, testClient);
      });
    });
  });
};

exports.destroyTestToken = function(callback){
	Token.remove({}, function(err){
    if(err) throw err;
    callback();
  });
};

exports.createTestUser = function(callback){
	var testUser = new User({
		first_name: 'DJ',
    last_name: 'Hayden',
    email: 'dj.hayden' + Math.floor((Math.random()*100)+1) + '@test.com',
    password: 'testpassword'
	}).save(function(err, result){
		if(err) throw err;
		callback(result);
	});
};

exports.destroyTestUser = function(callback){
	User.remove({}, function(err){
		if(err) throw err;
		callback();
	});
};
