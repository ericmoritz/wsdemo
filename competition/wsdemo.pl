#!/usr/bin/perl
use warnings;
use strict;

use Socket;
use Errno qw(EAGAIN);
use Fcntl;
use EV;
use IO::Stream;
use Protocol::WebSocket;
use Protocol::WebSocket::Handshake::Server;
use Protocol::WebSocket::Frame;


my ($host, $port) = ('0.0.0.0', 8000);
socket  my $srv_sock, AF_INET, SOCK_STREAM, 0;
setsockopt $srv_sock, SOL_SOCKET, SO_REUSEADDR, 1;
bind       $srv_sock, sockaddr_in($port, inet_aton($host));
listen     $srv_sock, SOMAXCONN;
fcntl      $srv_sock, F_SETFL, O_NONBLOCK;
my $srv_w = EV::io($srv_sock, EV::READ, sub {
    if (accept my $sock, $srv_sock) {
        IO::Stream->new({
            fh          => $sock,
            cb          => \&server,
            wait_for    => IN|EOF,
            HS          => Protocol::WebSocket::Handshake::Server->new,
            Frame       => Protocol::WebSocket::Frame->new,
        });
    }
    elsif ($! != EAGAIN) {
        die "accept: $!";
    }
});

EV::loop;

sub server {
    my ($io, $e, $err) = @_;
    if ($err) {
        $io->close();
#         warn $err;
    }
    elsif ($e & EOF) {
        $io->close();
    }
    elsif (!$io->{HS}->is_done) {
        $io->{HS}->parse($io->{in_buf});
        $io->{in_buf} = q{};
        my $hs_err = $io->{HS}->error;
        if ($hs_err) {
            $io->close();
            warn $hs_err;
        }
        elsif ($io->{HS}->is_done) {
            $io->write( $io->{HS}->to_string );
        }
    }
    else {
        $io->{Frame}->append( $io->{in_buf} );
        $io->{in_buf} = q{};
        while (my $msg = $io->{Frame}->next_bytes) {
            $io->write( $io->{Frame}->new($msg)->to_bytes );
        }
    }
}

