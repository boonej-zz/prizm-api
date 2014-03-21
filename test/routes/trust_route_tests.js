/**
 * Trust Route Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _mongoose     = require('mongoose'),
    _request      = require('request'),
    _chai         = require('chai'),
    _expect       = _chai.expect,
    _should       = _chai.should(),
    _assert       = _chai.assert,
    _thisapp      = require(process.cwd() + '/server'),
    _prism_home   = process.env.PRISM_HOME,
    _auth_model   = require(_prism_home + 'models/auth'),
    _users        = require(_prism_home + 'routes/users'),
    _trusts       = require(_prism_home + 'routes/trusts'),
    _helpers      = require(_prism_home + 'test/test_helpers'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post,
    Trust         = require(_prism_home + 'models/user').Trust;

describe('Trust Route Unit/Integration Tests ', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin;
  var follower = null;
  var followee = null;
  var follow_result = { followee: null, follower: null};
  var request_result = null;
  var test_user = null;
  var test_token = null;
  var test_client = null;
  var test_code = null;
  var test_post1 = null;
  var test_post2 = null;
  var test_post3 = null;
  var test_post4 = null;
  var test_post5 = null;
  var test_post6 = null;
  var test_post7 = null;
  var test_post8 = null;
  var test_comment_id = null;
  var test_comment_id2 = null;

  var executeRequest = function(method, url, body, cb){
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

  var setupSocialNetworkUsers = function(cb){
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

  var executeFollowRequest = function(u_follower, u_followee, cb){
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

  var executeLikeRequest = function(type, u_creator, post_id, cb){
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

  var executeCommentLikeRequest = function(type, u_creator, post_id, comment_id, cb){
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

  var executeAddCommentRequest = function(u_creator, post_id, cb){
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

  var executeDeleteCommentRequest = function(post_id, comment_id, cb){
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

  var executeDeletePostRequest = function(post_id, creator, cb){
    executeRequest('DELETE', 'posts/'+post_id, {creator: creator}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeFlagPostRequest = function(post_id, reporter_id, cb){
    executeRequest('POST', 'posts/'+post_id+'/flag', {reporter: reporter_id}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  var executeUpdatePostRequest = function(post_id, updated_post, cb){
    executeRequest('PUT', 'posts/'+post_id, updated_post, function(err, res){
      if(cb) cb(err,res);
    });
  };

  var executeCreateTrustRequest = function(user_id, creator, cb){
    executeRequest('POST', 'users/'+user_id+'/trusts', {creator: creator}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  before(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.createTestUser(function(testuser){
        test_user = testuser;

        _helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;
          setupSocialNetworkUsers(function(c){
            var social_network_followers = [  mark,
                                              edwardo,
                                              cameron,
                                              erica,
                                              sean,
                                              maryolin ];

            for(var i = 0; i < social_network_followers.length; i++){

              executeFollowRequest( social_network_followers[i],
                                    test_user,
                                    null);

              if(i !== 3) executeFollowRequest( test_user,
                                                social_network_followers[i],
                                                null );
            }
            //setup fake posts
            var posts = _helpers.fetchFakePostsArray(mark, test_user);
            Post.create(posts, function(err, test1, test2, test3, test4, test5, test6, test7, test8){
              test_post1 = test1;
              test_post2 = test2;
              test_post3 = test3;
              test_post4 = test4;
              test_post5 = test5;
              test_post6 = test6;
              test_post7 = test7;
              test_post8 = test8;
              // executeAddCommentRequest()
              done();
            });
          });
        });
      });
    });
  });

  after(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.destroyTestPost(function(){
        done();
      });
    });
  });

  describe('Testing Trust Request Validation Method', function(done){
    var executeFalseTrustRequest = function(type, cb){
      var url = 'users/'+test_user._id+'/trusts';
      if(type === 'DELETE' || type === 'PUT') url = url + '/asdfasdfasdfashfg';
      executeRequest(type, url, null, function(err, res){
        if(cb) cb(err, res);
      });
    };

    it('should send an error if creator is not sent in a post request', function(done){
      executeFalseTrustRequest('POST', function(err, response){
        _assert.isNull(err, 'Error on false trust request should be null');
        _expect(response.data.length).to.equal(0);
        _expect(response.metadata.success).to.equal(false);
        _expect(response.error.error).to.equal('invalid_request');
        done();
      });
    });
    it('should send an error if creator is not send in a put request', function(done){
      executeFalseTrustRequest('PUT', function(err, response){
        _assert.isNull(err, 'Error on false trust request should be null');
        _expect(response.data.length).to.equal(0);
        _expect(response.metadata.success).to.equal(false);
        _expect(response.error.error).to.equal('invalid_request');
        done();
      });
    });
    it('should send an error if creator is not send in a delete request', function(done){
      executeFalseTrustRequest('DELETE', function(err, response){
        _assert.isNull(err, 'Error on false trust request should be null');
        _expect(response.data.length).to.equal(0);
        _expect(response.metadata.success).to.equal(false);
        _expect(response.error.error).to.equal('invalid_request');
        done();
      });
    });
  });

  describe('Testing Creating a Trust Relationship', function(done){
    var trust, trust_error, test_sender, test_receiver;

    before(function(done){
      test_sender = mark;
      test_receiver = sean;
      executeCreateTrustRequest(test_receiver._id, test_sender._id, function(err, res){
        trust = res;
        trust_error = err;
        User.findOne({_id: test_sender}, function(err, result){
          if(err) throw err;
          test_sender = result;
          User.findOne({_id: test_receiver}, function(err, result){
            if(err) throw err;
            test_receiver = result;
            done();
          });
        });
      });
    });

    it('should successfully create and return (receivers) a trust object', function(done){
      _assert.isNull(trust_error, 'Trust Request error should be null');
      _expect(trust.metadata.success).to.equal(true);
      _expect(trust.data.length).to.equal(1);
      _expect(trust.data[0]).to.have.property('_id');
      _expect(trust.data[0]).to.have.property('user_id');
      _expect(trust.data[0].user_id._id.toString()).to.equal(test_sender._id.toString());
      _expect(trust.data[0]).to.have.property('status');
      _expect(trust.data[0]).to.have.property('is_owner');
      _expect(trust.data[0]).to.have.property('create_date');
      _expect(trust.data[0]).to.have.property('delete_date');
      _expect(trust.data[0]).to.have.property('modify_date');
      done();
    });
    it('should return an error if you try to create trust that already exists', function(done){
      executeCreateTrustRequest(test_receiver._id, test_sender._id, function(err, res){
        console.log(res);
        _expect(res.data.length).to.equal(0);
        _expect(res.error.error).to.equal('unable_to_create_trust');
        _expect(res.metadata.success).to.equal(false);
        done();
      });
    });
    it('should return a shortUser object in the trust objects user_id', function(done){
      _expect(trust.data[0].user_id).to.have.property('_id');
      _expect(trust.data[0].user_id).to.have.property('name');
      _expect(trust.data[0].user_id).to.have.property('first_name');
      _expect(trust.data[0].user_id).to.have.property('last_name');
      _expect(trust.data[0].user_id).to.have.property('profile_photo_url');
      done();
    });
    it('should add a trust object to the senders array', function(done){
      _expect(test_sender.trusts.length).to.equal(1);
      _expect(test_sender.trusts[0].user_id.toString()).to.equal(test_receiver._id.toString());
      done();
    });
    it('should set true for is_owner in senders trust_object', function(done){
      _expect(test_sender.trusts[0].is_owner).to.equal(true);
      done();
    });
    it('should set pending for status in senders trust object', function(done){
      _expect(test_sender.trusts[0].status).to.equal('pending');
      done();
    });
    it('should add a trust object the the receivers array', function(done){
      _expect(test_receiver.trusts.length).to.equal(1);
      _expect(test_receiver.trusts[0].user_id.toString()).to.equal(test_sender._id.toString());
      done();
    });
    it('should set false for is_owner in receivers trust_object', function(done){
      _expect(test_receiver.trusts[0].is_owner).to.equal(false);
      done();
    });
    it('should set pending for status in receivers trust object', function(done){
      _expect(test_receiver.trusts[0].status).to.equal('pending');
      done();
    });
    it('should increment the users trusts_count when a trust is created', function(done){
      executeCreateTrustRequest(mark._id, edwardo._id, function(err, result){
        _expect(result.metadata.success).to.equal(true);
        User.findOne({_id: mark._id}, function(err, refreshed){
          _expect(refreshed.trusts.length).to.equal(2);
          _expect(refreshed.trusts_count).to.equal(2);
          done();
        });
      });
    });
    describe('Testing Fetching a Users Trusts', function(done){
      var trusts, test_user;
      var executeFetchTrustsRequest = function(user_id, cb){
        executeRequest('GET', 'users/'+user_id+'/trusts', null, function(err, res){
          if(cb) cb(err, res);
        });
      };

      before(function(done){
        executeFetchTrustsRequest(mark._id.toString(), function(err, res){
          test_user = mark;
          trusts = res;
          done();
        });
      });

      it('should return a users trusts array successfully', function(done){
        _expect(trusts.metadata.success).to.equal(true);
        _expect(trusts.data.length).to.be.above(0);
        _expect(trusts.data[0]).to.have.property('trusts');
        _expect(trusts.data[0]).to.have.property('trusts_count');
        _expect(trusts.data[0].trusts_count).to.equal(2);
        _expect(trusts.data[0].trusts.length).to.equal(2);
        done();
      });
    });
    describe('Testing Updating a Users Trust', function(done){
      var approver, requestor, trust_to_update, associate_trust_to_update, update_result, update_error;

      var executeUpdateTrustRequest = function(user_id, trust_id, body, cb){
        executeRequest('PUT', 'users/'+user_id+'/trusts/'+trust_id, body, function(err, res){
          if(cb) cb(err ,res);
        });
      };

      var refreshTestUsers = function(cb){
         User.find({_id: {$in : [mark._id, sean._id]}}, function(err, res){
          mark = res[0];
          sean = res[1];
          cb();
        });
      };

      before(function(done){
        refreshTestUsers(function(){
          trust_to_update = sean.trusts[0];
          if(mark.trusts[0].user_id.toString() === sean._id.toString()) associate_trust_to_update = mark.trusts[0];
          if(mark.trusts[1].user_id.toString() === sean._id.toString()) associate_trust_to_update = mark.trusts[1];
          executeUpdateTrustRequest(  sean._id.toString(),
                                      trust_to_update._id.toString(),
                                      {status: 'accepted'},
                                      function(err, result){
                                        update_error = err;
                                        update_result = result;
                                        done();
                                      });
        });
      });
      it('should update a users requested trust successfully', function(done){
        _expect(update_result.metadata.success).to.equal(true);
        _expect(update_result.data.length).to.be.above(0);
        _expect(update_result.data[0].status).to.equal('accepted');
        done();
      });
      it('should update the associated users trust successfully', function(done){
        sean.refresh(function(user){
          sean = user;
          _expect(sean.trusts[0].status).to.equal('accepted');
          done();
        });
      });
      it('should not return an error with a successful update', function(done){
        _assert.isNull(update_error, 'UpdateError from Trust Update should be null');
        done();
      });
    });
    describe('Testing Fetching Users Trusts with filter options', function(done){
      var test_result = null;
      before(function(done){
        executeCreateTrustRequest(mark._id, cameron._id, function(err, result){
          if(err) throw err;
          test_result = result;
          done();
        });
      });
      it('should filter fetch by status on requested user', function(done){
        executeRequest('GET', 'users/'+mark._id+'/trusts?status=pending', null, function(err, res){
          _expect(res.metadata.success).to.equal(true);
          _expect(res.data[0].trusts.length).to.equal(2);
          _expect(res.data[0].trusts[0].status).to.equal('pending');
          _expect(res.data[0].trusts[1].status).to.equal('pending');
          done();
        });
      });
      it('should filter fetch by owner on requested user', function(done){
        executeRequest('GET', 'users/'+mark._id+'/trusts?owner=true', null, function(err, res){
          _expect(res.metadata.success).to.equal(true);
          _expect(res.data[0].trusts.length).to.equal(1);
          _expect(res.data[0].trusts[0].is_owner).to.equal(true);
          done();
        });
      });
      it('should show creator `shortUser` object when creator fetches a user profile', function(done){
        executeRequest('GET', 'users/'+mark._id+'?creator='+sean._id.toString(), null, function(err, res){
          _expect(res.data[0].trusts.length).to.equal(1);
          _expect(res.data[0].trusts[0]).to.have.property('_id');
          _expect(res.data[0].trusts[0]).to.have.property('user_id');
          _expect(res.data[0].trusts[0]).to.have.property('create_date');
          _expect(res.data[0].trusts[0]).to.have.property('modify_date');
          _expect(res.data[0].trusts[0]).to.have.property('status');
          _expect(res.data[0].trusts[0].user_id).to.have.property('name');
          _expect(res.data[0].trusts[0].user_id).to.have.property('_id');
          _expect(res.data[0].trusts[0].user_id).to.have.property('first_name');
          _expect(res.data[0].trusts[0].user_id).to.have.property('last_name');
          _expect(res.data[0].trusts[0].user_id).to.have.property('profile_photo_url');
          _expect(res.data[0].trusts[0].user_id._id.toString()).to.equal(sean._id.toString());
          done();
        });
      });
    });
  });





























});
