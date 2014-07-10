var _prism_home   = process.env.PRISM_HOME,
    // _mongoose     = require('mongoose'),
    _auth_models  = require(_prism_home + 'models/auth'),
    _utils        = require(_prism_home + 'utils'),
    Token         = _auth_models.Token,
    Client        = _auth_models.ClientApplication,
    Code          = _auth_models.Code,
    Post          = require(_prism_home + 'models/post').Post,
    User          = require(_prism_home + 'models/user').User;

var test_client, test_code, test_token;

var self = this;

exports.basicRequestObject = function(xargs) {
  if(!xargs) {
    xargs = {string: "string", num: 234563, ob: {a:1, b:2, c:3}, arr: [0,1,2,3]};
  }

  var req = {};
  req.headers = {};
  req.headers['x-arguments'] = self.packageXargs(xargs);
  return req;
};

exports.isHexObjectId = function(t_string) {
  var checkHex = new RegExp("^[0-9a-fA-F]{24}$");
  return checkHex.test(t_string);
};

exports.packageXargs = function(content) {
  var x = JSON.stringify(content);
  return new Buffer(x).toString('base64');
};

exports.unpackXargs = function(xargs) {
  return JSON.parse(new Buffer(xargs, 'base64').toString('utf8'));
};


exports.fetchTestToken = function(cb){
  if(!test_client && !test_code){
    this.createTestToken(function(){
      cb(test_token);
    });
  }else{
    cb(test_token);
  }
};

exports.executeRequest = function(method, url, body, cb){
  _request({
    method: method,
    strictSSL: false,
    json: true,
    url: 'https://localhost:3000/' + url,
    headers: {"Authorization":"Bearer " + test_token.access_token},
    body: (!body)? {} : body
  }, function(err,result){
    if(cb) cb(err, result.body);
  });
};

exports.setupSocialNetworkUsers = function(cb){
  var users = _helpers.fetchFakeUsersArray();
  User.create(users, function(err, m, e, c, e2, s, m2){
    if(err) throw err;
    mark = m;
    edwardo = e;
    cameron = c;
    erica = e2;
    sean = s;
    maryolin = m2;
    cb();
  });
};

exports.executeFollowRequest = function(u_follower, u_followee, cb){
  _request({
    method: 'POST',
    strictSSL: false,
    json: true,
    url: 'https://localhost:3000/users/'+u_followee._id+'/follow',
    headers: {"Authorization": 'Bearer ' + test_token.access_token},
    body: {creator: u_follower._id}
  }, function(err, result){
    if(cb) cb(err, result.body);
  });
};

exports.executeLikeRequest = function(type, u_creator, post_id, cb){
  _request({
    method: 'POST',
    strictSSL: false,
    json: true,
    url: 'https://localhost:3000/posts/'+post_id+'/'+type,
    headers: {"Authorization" : 'Bearer ' + test_token.access_token},
    body: {creator: u_creator}
  }, function(err, res){
    if(cb) cb(err, res.body);
  });
};

exports.executeCommentLikeRequest = function(type, u_creator, post_id, comment_id, cb){
  _request({
    method: "POST",
    strictSSL: false,
    json:true,
    url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id+'/'+type,
    headers: {'Authorization':'Bearer ' + test_token.access_token},
    body: {creator: u_creator}
  }, function(err, response){
    if(cb) cb(err, response.body);
  });
};

exports.executeAddCommentRequest = function(u_creator, post_id, cb){
  _request({
    method: 'POST',
    strictSSL: false,
    json: true,
    url: 'https://localhost:3000/posts/' + post_id + '/comments',
    headers: {"Authorization" : "Bearer " + test_token.access_token},
    body: {creator: u_creator, text: 'test commenting on this post'}
  }, function(err, result){
    if(cb) cb(err, result.body);
  });
};

exports.executeDeleteCommentRequest = function(post_id, comment_id, cb){
  _request({
    method: 'DELETE',
    strictSSL: false,
    json: true,
    url: 'https://localhost:3000/posts/'+post_id+'/comments/'+comment_id,
    headers: {'Authorization' : 'Bearer '+ test_token.access_token}
  }, function(err, result){
    if(cb) cb(err, result.body);
  });
};

exports.executeDeletePostRequest = function(post_id, creator, cb){
  executeRequest('DELETE', 'posts/'+post_id, {creator: creator}, function(err, res){
    if(cb) cb(err, res);
  });
};

exports.executeFlagPostRequest = function(post_id, reporter_id, cb){
  executeRequest('POST', 'posts/'+post_id+'/flag', {reporter: reporter_id}, function(err, res){
    if(cb) cb(err, res);
  });
};

exports.executeUpdatePostRequest = function(post_id, updated_post, cb){
  executeRequest('PUT', 'posts/'+post_id, updated_post, function(err, res){
    if(cb) cb(err,res);
  });
};


var fixtureUsersArray = [
  'mark',
  'edwardo',
  'cameron',
  'erica',
  'sean',
  'maryolin',
  'DJ'
];

exports.fetchFixturePosts = function(cb){
  Post.find({}, function(err, result){
    if(err) throw err;
    cb(result);
  });
};

exports.fetchFixtureTestUsers = function(cb){
  var users = {};
  User.find({first_name: {$in : fixtureUsersArray}}, function(err, result){
    if(err) throw err;
    for(var index in result){
      users[result[index].first_name] = result[index];
    }
    cb(users);
  });
};

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
            {text: 'test test tes1', creator: testUser._id, target_id: user._id, category: 'experiences', create_date: Date.now() - 10 * 60 * 1000 , scope: 'public'},
            {text: 'test test tes2', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() - 60 * 60 * 100, scope: 'public'},
            {text: 'test test tes3', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 60 * 60 * 1000, scope: 'public'},
            {text: 'test test tes4', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 1 * 60 * 60 * 1000, scope: 'public'},
            {text: 'test test tes5', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 45 * 60 * 1000, scope: 'public'},
            {text: 'test test tes6', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 10 * 60 * 1000, scope: 'public'},
            {text: 'test test tes7', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 2 * 10 * 60 * 1000, scope: 'public'},
            {text: 'test test tes8', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() - 1 * 60 * 60 * 1000, scope: 'public'},
            {text: 'test test tes9', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 10 * 60 * 60 * 1000, scope: 'public'},
            {text: 'test test tes10', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() - 10 * 60 * 60 * 1000, scope: 'public'},
            {text: 'test test tes11', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 30 * 60 * 1000, scope: 'public'},
            {text: 'test test tes12', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 15 * 60 * 1000, scope: 'public'},
            {text: 'test test tes13', creator: testUser._id, target_id: user._id, category: 'experiences',create_date: Date.now() + 10 * 60 * 100, scope: 'public'}
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
    test_client = client;
    var testCode = new Code({client_id: client.client_id});
    testCode.save(function(error, code){
      if(error) throw error;
      test_code = code;
      var testToken = new Token({code: code.code, client_application: client, grant_type: 'authorization_code'})
      .save(function(error, token){
        if(error) throw error;
        test_token = token;
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
		first_name: 'DONJUAN',
    last_name: 'TEST',
    email: 'dj.hayden.test' + Math.floor((Math.random()*100)+1) + '@test.com',
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

exports.destroyTestPost = function(callback){
  Post.remove({}, function(err){
    if(err) throw err;
    callback();
  });
};
