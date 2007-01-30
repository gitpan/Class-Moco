use ExtUtils::MakeMaker;
WriteMakefile(
    NAME              => 'Class::Moco',
    VERSION_FROM      => 'lib/Class/Moco.pm',
    PREREQ_PM         => {
        Class::Trigger => '',
        SQL::Abstract => '',
        DBI => '',
        DBD::SQLite => '',
        List::Util => '',
        List::MoreUtils => '',
        Exporter => '',
        Cache::Memory => '',
    },
    ($] >= 5.005 ?
      (ABSTRACT_FROM  => 'lib/Class/Moco.pm',
       AUTHOR         => 'Junya Kondo <jkondo@hatena.com>') : ()),
);
