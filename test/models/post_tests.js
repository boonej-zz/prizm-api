/**
 * Post Model Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request    = require('request'),
    _thisapp    = require(process.cwd() + '/server.js'),
    _mongoose   = require('mongoose'),
    _chai       = require('chai'),
    _expect     = _chai.expect,
    _should     = _chai.should(),
    _assert     = _chai.assert,
    _logger     = require('winston'),
    _helpers    = require(process.env.PRISM_HOME + 'test/test_helpers'),
    Post        = require(process.env.PRISM_HOME + 'models/post').Post,
    User        = require(process.env.PRISM_HOME + 'models/user').User;

describe('Post Model Unit Tests', function(done){
  var mark, edwardo, cameron, erica, sean, maryolin, test_user, test_post;

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

  before(function(done){
    _helpers.destroyTestUser(function(){
      _helpers.createTestUser(function(user){
        test_user = user;
        setupSocialNetworkUsers(function(){
          done();
        });
      });
    });
  });

  after(function(done){
    _helpers.destroyTestPost(function(){
      done();
    });
  });

	describe('Testing creating new post record', function(done){
		it('should create post record successfully with requried fields', function(done){
      var post = new Post({
        text : 'this is the body of the test post',
        category : 'experiences',
        target_id: test_user._id,
        creator : test_user._id
      });

      post.save(function(error, result){
        test_post = result;
        var date = Date.now();
        _expect(result).to.have.property('text');
        _expect(result).to.have.property('category');
        _expect(result).to.have.property('target_id');
        _expect(result).to.have.property('creator');
        _expect(result).to.have.property('create_date');
        _expect(result).to.have.property('modify_date');
        _expect(result).to.have.property('scope');
        _expect(result).to.have.property('status');
        _expect(result).to.have.property('_id');
        result.target_id.should.equal(test_user._id.toString());
        result.creator.should.equal(test_user._id);
        result.create_date.valueOf().should.be.within(date.valueOf() -5, date.valueOf() +5);
        result.modify_date.valueOf().should.be.within(date.valueOf() -5, date.valueOf() +5);
        result.scope.should.equal('public');
        result.status.should.equal('active');
        done();
			});
		});
	});
  describe('Testing reporting an inappropriate post', function(done){
    var reporter_post;

    before(function(done){
      var reporter = {reporter_id: mark._id, create_date: Date.now()};
      test_post.flagged_reporters.push(reporter);
      test_post.save(function(err, result){
        if(err) throw err;
        reporter_post = result;
        done();
      });
    });
    it('should add a reporter id & date to flagged_reporters attr', function(done){
      _expect(reporter_post.flagged_reporters.length).to.equal(1);
      _expect(reporter_post.flagged_reporters[0]).to.have.property('reporter_id');
      _expect(reporter_post.flagged_reporters[0]).to.have.property('create_date');
      _expect(reporter_post.flagged_reporters[0].reporter_id).to.equal(mark._id.toString());
      done();
    });
    it('should update is_flagged to true when saved', function(done){
      _expect(reporter_post.is_flagged).to.equal(true);
      done();
    });
    it('should increment flagged_count when saved', function(done){
      _expect(reporter_post.flagged_count).to.be.above(0);
      done();
    });
    it('should update the post status if 5 or more flags', function(done){
      reporter_post.flagged_reporters.push({reporter_id: sean._id, create_date: Date.now()});
      reporter_post.flagged_reporters.push({reporter_id: edwardo._id , create_date: Date.now()});
      reporter_post.flagged_reporters.push({reporter_id: erica._id, create_date: Date.now()});
      reporter_post.flagged_reporters.push({reporter_id: maryolin._id, create_date: Date.now()});
      reporter_post.save(function(err, result){
        if(err) throw err;
        _expect(result.flagged_count).to.equal(5);
        _expect(result.flagged_reporters.length).to.equal(5);
        _expect(result.status).to.equal('review');
        done();
      });
    });
  });
});
