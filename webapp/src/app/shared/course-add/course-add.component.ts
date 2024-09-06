import { Component, model, output } from '@angular/core';
import { Kolegij } from '../../core';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-course-add',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './course-add.component.html',
  styleUrl: './course-add.component.scss',
})
export class CourseAddComponent {
  data = model.required<Kolegij>();
  query = output<Kolegij>();

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
}
