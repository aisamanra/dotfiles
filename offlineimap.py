#!/usr/bin/python2


from subprocess import check_output


def passwd(account):
    return check_output('pass gmail/{0}'.format(account),
                        shell=True).splitlines()[0]
