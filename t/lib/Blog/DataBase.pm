package Blog::DataBase;
use strict;
use warnings;
use base qw(Class::Moco::DataBase);

my $DB;
use File::Temp qw/tempfile/;
(undef, $DB) = tempfile();

__PACKAGE__->dsn("dbi:SQLite:dbname=$DB");

__PACKAGE__->execute(<<EOF);
CREATE TABLE user (
  user_id INTEGER PRIMARY KEY,
  name varchar(255)
)
EOF

my @users = (
    [qw(1 jkondo)],
    [qw(2 reikon)],
    [qw(3 cinnamon)],
);
__PACKAGE__->execute('insert into user values (?,?)',undef,$_) for @users;

__PACKAGE__->execute(<<EOF);
CREATE TABLE entry (
  entry_id INTEGER PRIMARY KEY,
  user_id INTEGER,
  title text,
  body text
)
EOF

my @entries = (
    [qw(1 1 jkondo-1 hello)],
    [qw(2 1 jkondo-2 world)],
    [qw(3 2 reikon-1 hello)],
    [qw(4 3 cinnamon-1 dog)],
);
__PACKAGE__->execute('insert into entry values (?,?,?,?)',undef,$_)
    for @entries;

__PACKAGE__->execute(<<EOF);
CREATE TABLE bookmark (
  user_id INTEGER,
  entry_id INETEGER,
  PRIMARY KEY(user_id,entry_id)
)
EOF

my @bookmarks = ([1,3], [1,4], [2,1], [2,2], [3,2]);
__PACKAGE__->execute('insert into bookmark values (?,?)',undef,$_)
    for @bookmarks;

sub DESTROY {
    my $class = shift;
    $class->dbh->disconnect;
    unlink $DB if -e $DB;
}

1;
