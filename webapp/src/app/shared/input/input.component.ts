import { Component, input, model } from '@angular/core';

type InputType = 'email' | 'password' | 'text' | 'textarea';

type Width = `${number}px` | `${number}%` | `${number}vw`;
type Height = `${number}px` | `${number}%` | `${number}vh`;
type FontSize = `${number}px`;

type InputStyle = {
  width: Width;
  height: Height;
  font_size?: FontSize;
};

@Component({
  selector: 'app-input',
  standalone: true,
  imports: [],
  templateUrl: './input.component.html',
  styleUrl: './input.component.scss',
})
export class InputComponent {
  value = model<string>('');
  type = input<InputType>('text');
  placeholder = input<string>('');
  style = input<InputStyle>({
    width: '150px',
    height: '30px',
    font_size: '22px',
  });

  change(value: string): void {
    this.value.set(value);
  }
}
