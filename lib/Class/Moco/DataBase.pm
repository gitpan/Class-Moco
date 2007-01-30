package Class::Moco::DataBase;
use strict;
use warnings;
use base qw (Class::Data::Inheritable);
use DBI;

__PACKAGE__->mk_classdata($_) for qw(dsn username password last_insert_id);

sub dbh {
    my $class = shift;
    my $vname = $class . '::dbh';
    no strict 'refs';
    $$vname ||= DBI->connect(
        $class->dsn, $class->username, $class->password
    );
    return $$vname;
}

sub execute {
    my $class = shift;
    my ($sql, $data, $binds) = @_;
    $sql or return;
    my @bind_values = ref $binds eq 'ARRAY' ? @$binds : ();
    my $dbh = $class->dbh;
    my $sth = @bind_values ? $dbh->prepare_cached($sql,undef,1) :
        $dbh->prepare($sql);
    unless ($sth) { warn $dbh->errstr and return; }
    if (defined $data) {
        $sth->execute(@bind_values) or 
            warn sprintf('SQL Error: "%s" (%s)', $sql, $sth->errstr) and return;
        $$data = $sth->fetchall_arrayref({});
    } else {
        unless ($sth->execute(@bind_values)) {
            warn qq/SQL Error "$sql"/;
            return;
        }
    }
    if ($sql =~ /^insert/io) {
        $class->last_insert_id($dbh->last_insert_id(undef,undef,undef,undef));
    }
    return !$sth->err;
}

1;

=head1 NAME

Class::Moco::DataBase - Data Base Handler for Class::Moco

=head1 SYNOPSIS

  package MyDataBase;
  use base qw(Class::Moco::DataBase);

  __PACKAGE__->dsn('dbi:mysql:myapp');
  __PACKAGE__->username('test');
  __PACKAGE__->password('test');

  1;

  # In your scripts
  MyDataBase->execute('select 1');

=head1 SEE ALSO

L<Class::Moco>

=head1 AUTHOR

Junya Kondo, E<lt>jkondo@hatena.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) Hatena Inc. All Rights Reserved.

This library is free software; you may redistribute it and/or modify
it under the same terms as Perl itself.

=cut
