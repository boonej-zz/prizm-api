// Profile Helper
var _ = require('underscore');

var shuffle = function(o){
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

/* This suffles the profile header images and replaces part of 
the string to use a thumbnail image rather than full size image */
exports.shufflePostImagesForProfileHeader = function(posts) {
  if (posts.length < 16) {
    return posts;
  }
  else {
    var profileImages = [];
    var thumb_file_path;
    _.each(posts, function(post, index, list) {
      thumb_file_path = post.file_path.replace('.jpg', '_4.jpg');
      profileImages[index] = thumb_file_path;
    });
    var shuffledImages = shuffle(profileImages);
    return shuffledImages;
  }
};



