package App::Clk::Storage::SQLite;
use strict;
use warnings;
use DBI;

sub open {
    my ($class) = @_;
    return bless {}, $class;
}

sub dbh {
    my ($self) = @_;
    my $dbh = $self->{dbh};
    return $dbh if $dbh and $dbh->ping;

    my $creating = 0;
    if ( not -d 'sqlite' ) {
        mkdir 'sqlite';
        $creating = 1;
    }
    $dbh = $self->{dbh} = DBI->connect('dbi:SQLite:sqlite/db');
    $dbh->do(q{
        CREATE TABLE universal_timeline (
            id      INTEGER PRIMARY KEY AUTOINCREMENT,
            owner   TEXT NOT NULL,
            time    TEXT NOT NULL,
            subject TEXT,
            tags    TEXT
        )
    }) if $creating;
    return $dbh;
}

sub add {
    my ($self, $event) = @_;
    my $sql = q{
        INSERT INTO universal_timeline (owner, time, subject, tags)
        VALUES (?,?,?,?)
    };
    my $count = $self->dbh->do(
        $sql,
        undef,
        $event->get_owner, $event->get_time, $event->get_subject,
        $event->get_tags_string,
    );
    return if $count == 1;
    die "Didn't add one entry to SQLite storage: $count\n";
}

1;
