import { Component, model, output } from '@angular/core';
import { Sadrzaj } from '../../core';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-content-edit',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './content-edit.component.html',
  styleUrl: './content-edit.component.scss',
})
export class ContentEditComponent {
  data = model.required<Sadrzaj>();
  query = output<Sadrzaj>();

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  editValue(field: string, value: unknown): void {
    this.data.update((d) => {
      d.vrijednost[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
}
