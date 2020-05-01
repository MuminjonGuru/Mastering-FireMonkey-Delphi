See the animation here: https://github.com/MuminjonGuru/FireMonkey-powered-Login-form-with-animations./blob/master/AnimateFloat/TAnimator.gif

*TAnimator* from **FMX.Ani**

     TAnimator.AnimateFloat(RctAnimate, 'Height', 100, 0.8,
                            TAnimationType.&In, TInterpolationType.Linear);

> Collapsible & Expendable Animated TRectangle
> Main Logic:

    procedure TForm1.BtnActionClick(Sender: TObject);
    begin
      if RctAnimate.Height = 250 then
      begin
        TAnimator.AnimateFloat(RctAnimate, 'Height', 100, 0.8,
                              TAnimationType.&In, TInterpolationType.Linear);
        BtnAction.Text := 'Expand';
      end
      else
      begin
        TAnimator.AnimateFloat(RctAnimate, 'Height', 250, 0.8,
                              TAnimationType.&In, TInterpolationType.Linear);
        BtnAction.Text := 'Collapse';
      end;
    end; 


You can control ShowScrollBar property of the VertScrollBarClient for UI purposes.
