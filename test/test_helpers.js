var _prism_home		= process.env.PRISM_HOME
	, _auth_models	= require(_prism_home + 'models/auth')
	, _utils				= require(_prism_home + 'utils')
	, Token					= _auth_models.Token
  , Client 				= _auth_models.ClientApplication
  , Code 					= _auth_models.Code
  , User					= require(_prism_home + 'models/user').User;


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
