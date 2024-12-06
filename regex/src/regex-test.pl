########################## perl regex 测试脚本

#***************************** 定义测试函数
sub te_1 {
    my $test_doc = <<~EOFFF;
    abs(1.0)
    fabs(1.0)
    gabs(1.0)
    habs(1.0)
    std::abs(1.0)
    std::fabs(1.0)
    std::std::abs(1.0)
    std::std::fabs(4.0)
    EOFFF
    # 
    foreach (split(/\n/, $test_doc))
    {
        chomp;
        $a = $_;
        $_ =~ s/(?<!std::|f)abs\(/std::abs(/g;
        # $_ =~ s/(?<!(std::|f))abs\(/std::abs(/g; # wrong    
        # $_ =~ s/(?<!std::)(?<!f)abs\(/std::abs(/g; # ok
        # $_ =~ s#abs\(#wabs(#g;
        print "$a\t->\t$_\n";
    }
}

#*************************************** run test
te_1()