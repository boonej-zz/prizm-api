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
    _             = require('underscore'),
    User          = require(_prism_home + 'models/user').User,
    Post          = require(_prism_home + 'models/post').Post,
    Trust         = require(_prism_home + 'models/trust').Trust;

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

  var executeCreateTrustRequest = function(user_id, creator, cb){
    executeRequest('POST', 'users/'+user_id+'/trusts', {creator: creator}, function(err, res){
      if(cb) cb(err, res);
    });
  };

  before(function(done){
    Trust.remove({}, function(err){
      if(err) throw err;
      _helpers.createTestUser(function(testuser){
        test_user = testuser;
        _helpers.createTestToken(function(token, code, client){
          test_token = token;
          test_code = code;
          test_client = client;
          _helpers.fetchFixtureTestUsers(function(users){
            mark = users.mark;
            edwardo = users.edwardo;
            cameron = users.cameron;
            erica = users.erica;
            sean = users.sean;
            maryolin = users.maryolin;
            DJ = users.DJ;
            //setup fake posts
            _helpers.fetchFixturePosts(function(posts){
              test_post1 = posts[0];
              test_post2 = posts[1];
              test_post3 = posts[2];
              test_post4 = posts[3];
              test_post5 = posts[4];
              test_post6 = posts[5];
              test_post7 = posts[6];
              test_post8 = posts[7];
              done();
            });
          });
        });
      });
    });
  });

  after(function(done){
    User.remove({_id: test_user._id}, function(err , result){
      if(err) throw err;
      done();
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
    //TODO fix & update
    it.skip('should error if creator is not in PUT request', function(done){
      executeFalseTrustRequest('PUT', function(err, response){
        _assert.isNull(err, 'Error on false trust request should be null');
        _expect(response.data.length).to.equal(0);
        _expect(response.metadata.success).to.equal(false);
        _expect(response.error.error).to.equal('invalid_request');
        done();
      });
    });
    //TODO fix & update
    it.skip('should error if creator is not in DELETE request', function(done){
      executeFalseTrustRequest('DELETE', function(err, response){
        _assert.isNull(err, 'Error on false trust request should be null');
        _expect(response.data.length).to.equal(0);
        _expect(response.metadata.success).to.equal(false);
        _expect(response.error.error).to.equal('invalid_request');
        done();
      });
    });
  });

  describe('Creating a Trust', function(done){
    var trust, trust_error, full_trust, test_sender, test_receiver;

    before(function(done){
      test_sender = mark;
      test_receiver = sean;
      executeCreateTrustRequest(test_receiver._id, test_sender._id, function(err, res){
        trust = res;
        trust_error = err;
        Trust.findOne({_id: res._id}, function(err, full){
          full_trust = full;
          done();
        });
      });
    });

    it('should successfully create and return a trust object', function(done){
      _assert.isNull(trust_error, 'Trust Request error should be null');
      _expect(trust.metadata.success).to.equal(true);
      _expect(trust.data.length).to.equal(1);
      _expect(trust.data[0]).to.have.property('_id');
      _expect(trust.data[0]).to.have.property('to');
      _expect(trust.data[0]).to.have.property('from');
      _expect(trust.data[0]).to.have.property('to_posts');
      _expect(trust.data[0]).to.have.property('from_posts');
      _expect(trust.data[0]).to.have.property('to_comments');
      _expect(trust.data[0]).to.have.property('from_comments');
      _expect(trust.data[0]).to.have.property('to_post_likes');
      _expect(trust.data[0]).to.have.property('to_comment_likes');
      _expect(trust.data[0]).to.have.property('from_comment_likes');
      _expect(trust.data[0]).to.have.property('from_post_likes');
      _expect(trust.data[0]).to.have.property('from_posts_count');
      _expect(trust.data[0]).to.have.property('to_posts_count');
      _expect(trust.data[0]).to.have.property('from_comments_count');
      _expect(trust.data[0]).to.have.property('to_comments_count');
      _expect(trust.data[0]).to.have.property('from_likes_count');
      _expect(trust.data[0]).to.have.property('to_likes_count');
      _expect(trust.data[0]).to.have.property('status');
      _expect(trust.data[0]).to.have.property('type');
      _expect(trust.data[0]).to.have.property('create_date');
      _expect(trust.data[0]).to.have.property('delete_date');
      _expect(trust.data[0]).to.have.property('modify_date');
      _expect(trust.data[0].to.toString()).to.equal(test_receiver._id.toString());
      _expect(trust.data[0].from.toString()).to.equal(test_sender._id.toString());
      done();
    });
    it('should error if you try to create trust that already exists', function(done){
      executeCreateTrustRequest(test_receiver._id, test_sender._id, function(err, res){
        _expect(res.data.length).to.equal(0);
        _expect(res.error.error).to.equal('unable_to_create_trust');
        _expect(res.metadata.success).to.equal(false);
        done();
      });
    });
    it('should set pending for the trust status', function(done){
      _expect(trust.data[0].status).to.equal('pending');
      done();
    });
    it('should set a default of 0 for all property counts', function(done){
      var data = trust.data[0];
      _expect(data.from_posts_count).to.equal(0);
      _expect(data.from_comments_count).to.equal(0);
      _expect(data.from_likes_count).to.equal(0);
      _expect(data.to_posts_count).to.equal(0);
      _expect(data.to_comments_count).to.equal(0);
      _expect(data.to_likes_count).to.equal(0);
      done();
    });
    it('should find trust via static Trust.findTrust method', function(done){
      Trust.findTrust(test_receiver._id, test_sender._id, function(err, trust){
        _assert.isNull(err, 'findTrust Error should be  null when trust exists');
        var includes_both_users = false;
        if( ( trust.to.toString() === test_receiver._id.toString() &&
              trust.from.toString() === test_sender._id.toString() ) ||
            ( trust.to.toString() === test_sender._id.toString() &&
              trust.from.toString() === test_receiver._id.toString() ))
          includes_both_users = true;
        _assert.isTrue(includes_both_users, 'Both users should exist in trust');
        done();
      });
    });

    describe('Testing Update to Trust via Route', function(done){
      it('should allow you to change the type on update', function(done){
        var id = trust.data[0]._id.toString();
        executeRequest('PUT', 
                       'trusts/'+id, 
                       {status: "accepted", type: 'accolade'},
                       function(err, res){

          var result = res.data[0];
          _expect(result._id.toString()).to.equal(id);
          _expect(result.type).to.equal('accolade');
          _expect(result.status).to.equal('accepted');
          done();
        });
      });
    });

    //TODO: fix & update
    describe.skip('Testing Fetching a Users Trusts', function(done){
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
    describe.skip('Updating a Users Trust', function(done){
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
    describe.skip('Testing Fetching Users Trusts with filter options', function(done){
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

