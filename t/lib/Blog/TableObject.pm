package Blog::TableObject;
use strict;
use warnings;
use base qw 'Class::Moco';
use Blog::DataBase;

__PACKAGE__->db_object('Blog::DataBase');

1;
