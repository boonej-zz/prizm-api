/**
 * Post Model Unit Tests
 *
 * @author DJ Hayden <dj.hayden@stablekernel.com>
 */
var _request   = require('request')
  , _thisapp   = require(process.cwd() + '/server.js')
  , _mongoose  = require('mongoose')
  , _chai      = require('chai')
  , _expect    = _chai.expect
  , _should    = _chai.should()
  , _assert    = _chai.assert
  , _logger 	 = require('winston')
  , _helpers   = require(process.env.PRISM_HOME + 'test/test_helpers')
  , Post 			 = require(process.env.PRISM_HOME + 'models/post').Post
  , User       = require(process.env.PRISM_HOME + 'models/user').User;

describe('Post Model Unit Tests', function(done){
	describe('Testing creating new post record', function(done){
		it('should create post record successfully with requried fields', function(done){
      _helpers.createTestUser(function(user){

        var post = new Post({
          text : 'this is the body of the test post',
          category : 'post',
          target_id: user._id,
          creator : {id: user._id, name: 'Richard Tester'}
        });

          post.save(function(error, result){
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
            result.target_id.should.equal(user._id.toString());
            result.creator.id.should.equal(user._id.toString());
            result.creator.name.should.not.equal('Richard Tester');
            result.creator.name.should.equal(user.first_name + ' ' + user.last_name);
            result.create_date.valueOf().should.be.within(date.valueOf() -5, date.valueOf() +5);
            result.modify_date.valueOf().should.be.within(date.valueOf() -5, date.valueOf() +5);
            result.scope.should.equal('public');
            result.status.should.equal('active');
            done();
        });
			});
		});
	});
});
