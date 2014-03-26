/**
 * Activity Model Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _thisapp    = require(process.cwd() + '/server.js'),
    _request    = require('request'),
    _mongoose   = require('mongoose'),
    _chai       = require('chai'),
    _expect     = _chai.expect,
    _should     = _chai.should(),
    _assert     = _chai.assert,
    _logger     = require('winston'),
    _helpers    = require(process.env.PRISM_HOME + 'test/test_helpers'),
    Post        = require(process.env.PRISM_HOME + 'models/post').Post,
    User        = require(process.env.PRISM_HOME + 'models/user').User,
    Activity    = require(process.env.PRISM_HOME + 'models/activity').Activity,
    Trust       = require(process.env.PRISM_HOME + 'models/user').Trust;

describe('Activity Model Unit Tests', function(done){
  var test_user;
  var test_target_user;
  var test_activity;
  var test_activity_failed;
  var test_activity_success;
  var test_activity_success_date;
  var test_token;
  var test_code;
  var test_client;

  var setupUsers = function(cb){
    var u = new User({
      first_name: 'the',
      last_name: 'godfather',
      email: 'tgf@italy.com',
      password: 'horsesHead'
    }).save(function(err, user){
      if(err) throw err;
      test_user = user;
      var u2 = new User({
        first_name: '2cool',
        last_name: 'chris',
        email: '2cool@thenineties.com',
        password: 'oldschool'
      }).save(function(err, user2){
        if(err) throw err;
        test_target_user = user2;
        cb();
      });
    });
  };

  var executeFailedActivity = function(cb){
    new Activity({}).save(function(err){
      test_activity_failed = err;
      cb();
    });
  };

  var executeSuccessActivity = function(cb){
    new Activity({
      type: 'follow',
      user: test_user,
      target: test_target_user
    }).save(function(err, result){
      if(err) throw err;
      test_activity_success_date = Date.now();
      test_activity_success = result;
      cb();
    });
  };

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

  before(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.createTestToken(function(token, code, client){
        test_token = token;
        test_code = code;
        test_client = client;
        setupUsers(function(){
          executeFailedActivity(function(){
            executeSuccessActivity(function(){
              done();
            });
          });
        });
      });
    });
  });

  after(function(done){
    Activity.remove({}, function(){
      done();
    });
  });

  describe('Creating an Activity', function(done){
    it('should create an activity successfully', function(done){
      _expect(test_activity_success).to.have.property('_id');
      _expect(test_activity_success._id.toString().length).to.be.above(0);
      done();
    });
    it('should set the create_date on creation', function(done){
      var date = test_activity_success_date.valueOf();
      _expect(test_activity_success).to.have.property('create_date');
      _expect(test_activity_success).to.not.equal(null);
      _expect(test_activity_success).to.not.equal(false);
      _expect(test_activity_success).to.not.equal('undefined');
      _expect(test_activity_success.create_date.valueOf()).to.be.within(date-5, date+5);
      done();
    });
    it('should require an activity type', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('type');
      _expect(err.errors.type.type).to.equal('required');
      done();
    });
    it('should require an activity user', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('user');
      _expect(err.errors.user.type).to.equal('required');
      done();
    });
    it('should require an activity target user', function(done){
      var err = test_activity_failed;
      _expect(err.message).to.equal('Validation failed');
      _expect(err.errors).to.have.property('target');
      _expect(err.errors.target.type).to.equal('required');
      done();
    });
  });
  describe('Testing a User Activity', function(done){
    it('should track when a user creates a post', function(done){
      // process.on('activity_post_success', function(activity){
      //   _expect(activity).to.have.property('type');
      //   _expect(activity.type).to.equal('post');
      // });
      var body = {
        text: 'test tetst',
        creator: test_user._id,
        category: 'experiences',
        target_id: test_target_user._id
      };
      executeRequest('POST', 'users/'+test_target_user._id+'/posts', body, function(err, result){
        done();
      });
    });
  });


















});
