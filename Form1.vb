
Public Class Form1
    Dim sc As Integer = 0
    Dim CVS As PowerPacks.ShapeContainer
    Dim S(18, 18) As Byte
    Dim now As Color = Color.Black

    Private Check As Integer
    Private temp_x As Integer
    Private temp_y As Integer
    Private pointTemp As Point
    Public Sub New()
        '無框拖曳
        AddHandler MyBase.MouseDown, New MouseEventHandler(AddressOf Me.Form1_MouseDown)
        AddHandler MyBase.Load, New EventHandler(AddressOf Me.Form1_Load)
        AddHandler MyBase.MouseMove, New MouseEventHandler(AddressOf Me.Form1_MouseMove)
        AddHandler MyBase.MouseUp, New MouseEventHandler(AddressOf Me.Form1_MouseUp)
        Me.InitializeComponent()
    End Sub
    Private Sub Form1_MouseDown(ByVal sender As Object, ByVal e As MouseEventArgs)
        If (e.Button = MouseButtons.Left) Then
            Me.Check = 1
            Me.temp_x = e.X
            Me.temp_y = e.Y
        End If
    End Sub
    Private Sub Form1_MouseMove(ByVal sender As Object, ByVal e As MouseEventArgs)
        If ((Me.Check = 1) And (e.Button = MouseButtons.Left)) Then
            Me.pointTemp.X = (Me.pointTemp.X + (e.X - Me.temp_x))
            Me.pointTemp.Y = (Me.pointTemp.Y + (e.Y - Me.temp_y))
            Me.Location = Me.pointTemp
        End If
    End Sub
    Private Sub Form1_MouseUp(ByVal sender As Object, ByVal e As MouseEventArgs)
        If (e.Button = MouseButtons.Left) Then
            Me.Check = 0
            Me.temp_x = 0
            Me.temp_y = 0
        End If
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.TransparencyKey = System.Drawing.SystemColors.Control



        Dim bg As New Bitmap(760, 760)
        Dim g As Graphics = Graphics.FromImage(bg)
        g.Clear(Color.DarkOrange)
        For i As Integer = 15 To 555 Step 30
            g.DrawLine(Pens.Black, i, 15, i, 555)
        Next
        For j As Integer = 15 To 555 Step 30
            g.DrawLine(Pens.Black, 15, j, 555, j)
        Next
        Panel1.BackgroundImage = bg
        CVS = New PowerPacks.ShapeContainer
        Me.Panel1.Controls.Add(CVS)
    End Sub

    Private Sub Panel1_MouseDown(sender As Object, e As MouseEventArgs) Handles Panel1.MouseDown
        Dim i As Integer = e.X \ 30
        Dim j As Integer = e.Y \ 30
        If S(i, j) = 0 Then
            Chess(i, j, now)
            If now = Color.Black Then
                S(i, j) = 1
                now = Color.White
                'S(i, j) = 2
                '原本白子
                '現在要寫AI下棋
            End If
            AI()
            now = Color.Black
            If chk5(i, j, 1) Then
                MsgBox("黑棋勝！")
            ElseIf chk5(i, j, 2) Then
                MsgBox("白棋勝！")
            End If
        End If
    End Sub
    Private Sub Chess(i As Integer, j As Integer, BW As Color)
        Dim C As New PowerPacks.OvalShape
        C.Width = 26
        C.Height = 26
        C.Left = i * 30 + 2
        C.Top = j * 30 + 2
        C.FillStyle = PowerPacks.FillStyle.Solid
        C.FillColor = BW
        C.Parent = CVS
    End Sub
    Private Function chk5(ByVal i As Integer, ByVal j As Integer, tg As Byte) As Boolean
        Dim n As Integer = 0
        Dim ii As Integer, jj As Integer
        For k = -4 To 4
            ii = i + k
            If ii >= 0 And ii <= 18 Then
                If S(ii, j) = tg Then
                    n += 1
                    If n = 5 Then Return True
                Else
                    n = 0
                End If
            End If
        Next
        n = 0
        For k = -4 To 4
            jj = j + k
            If jj >= 0 And jj <= 18 Then
                If S(i, jj) = tg Then
                    n += 1
                    If n = 5 Then Return True
                Else
                    n = 0
                End If
            End If
        Next
        n = 0
        For k = -4 To 4
            ii = i + k
            jj = j + k
            If ii >= 0 And ii <= 18 And jj >= 0 And jj <= 18 Then
                If S(ii, jj) = tg Then
                    n += 1
                    If n = 5 Then Return True
                Else
                    n = 0
                End If
            End If
        Next
        n = 0
        For k = -4 To 4
            ii = i - k
            jj = j + k
            If ii >= 0 And ii <= 18 And jj >= 0 And jj <= 18 Then
                If S(ii, jj) = tg Then
                    n += 1
                    If n = 5 Then Return True
                Else
                    n = 0
                End If
            End If
        Next
        Return False
    End Function

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        CVS.Shapes.Clear()
        ReDim S(18, 18)
    End Sub
    Private Sub AI()
        Dim X(18, 18) As Double
        Dim max As Short
        Dim a, b As Short
        '每一格都計算分數 (不包含以下子地方) S(i,j) = 0
        'S(i,j) = 1 擋活三 洞三 死四 優先 活二 死三 次
        'S(i,j) = 2 連成 五連 優先 次 四連 洞五 再次 活三 洞三 死四 再再次 活二 死三
        '權值計算問題
        sc = 0
        For i As Integer = 0 To 18
            For j As Integer = 0 To 18
                '按照位置做加分計算
                If S(i, j) = 0 Then
                    X(i, j) = chkl(i, j, 1) * chkl(i, j, 2)
                    '(-1 * Math.Sqrt(Math.Abs(9 - i) * Math.Abs(9 - j))) +
                Else
                    X(i, j) = -1
                End If

            Next
        Next
        max = X(0, 0)
        a = 0 : b = 0
        For i = 0 To 18
            For j = 0 To 18
                If j = 0 And i <> 0 Then
                    If X(i, j) > X(i - 1, 18) Then
                        max = X(i, j)
                        a = i
                        b = j
                    End If
                ElseIf j <> 0 And i <> 0 Then
                    If X(i, j) > X(i, j - 1) Then
                        max = X(i, j)
                        a = i
                        b = j
                    End If
                End If
            Next
        Next
        '下棋
        If S(a, b) = 0 Then
            Chess(a, b, now)
            S(a, b) = 2
        Else
            AI()
        End If
        If chk5(a, b, 2) Then
            MsgBox("白棋勝！")
        End If
    End Sub
    Private Function chkl(ByVal i As Integer, ByVal j As Integer, tg As Byte) As Short
        Dim ii As Integer, jj As Integer
        For k = -4 To 4
            ii = i + k
            If ii >= 0 And ii <= 18 Then
                If S(ii, j) = tg Then
                    sc += 1
                End If
            End If
        Next
        For k = -4 To 4
            jj = j + k
            If jj >= 0 And jj <= 18 Then
                If S(i, jj) = tg Then
                    sc += 1
                End If
            End If
        Next
        For k = -4 To 4
            ii = i + k
            jj = j + k
            If ii >= 0 And ii <= 18 And jj >= 0 And jj <= 18 Then
                If S(ii, jj) = tg Then
                    sc += 1
                End If
            End If
        Next
        For k = -4 To 4
            ii = i - k
            jj = j + k
            If ii >= 0 And ii <= 18 And jj >= 0 And jj <= 18 Then
                If S(ii, jj) = tg Then
                    sc += 1
                End If
            End If
        Next
        Return sc
    End Function

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Application.Exit()
    End Sub
End Class
